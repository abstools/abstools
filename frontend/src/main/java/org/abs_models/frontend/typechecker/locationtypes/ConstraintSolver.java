package org.abs_models.frontend.typechecker.locationtypes;

import org.abs_models.frontend.analyser.ErrorMessage;
import org.abs_models.frontend.analyser.SemanticConditionList;
import org.abs_models.frontend.analyser.TypeError;
import org.abs_models.frontend.ast.ASTNode;
import org.abs_models.frontend.typechecker.ext.AdaptDirection;

import java.util.*;

import static org.abs_models.frontend.typechecker.locationtypes.LocationTypeVar.*;

/**
 * Tries to find an optimal mapping from variables to location types in order to satisfy all constraints
 */
public class ConstraintSolver {
    private final ConstraintCollection constraints;
    private final SemanticConditionList errors = new SemanticConditionList();
    private final Map<LocationTypeVar, LocationTypeVar> rewritten = new HashMap<>();
    private final Set<LocationTypeVar> allVars;

    private final List<Update> updates = new ArrayList<>();

    private boolean debug;

    public ConstraintSolver(ConstraintCollection cs, boolean debug) {
        this.constraints = cs;
        this.allVars = new HashSet<>(cs.getVars());
        this.debug = debug;
    }

    /**
     * Tries to find an optimal mapping from variables to location types in order to satisfy all constraints
     * @return - The found mapping
     */
    public Map<LocationTypeVar, LocationType> solve() {
        Map<LocationTypeVar, LocationType> resolved = new HashMap<>();
        resolved.put(BOTTOM, LocationType.BOTTOM);
        resolved.put(NEAR, LocationType.NEAR);
        resolved.put(FAR, LocationType.FAR);
        resolved.put(SOMEWHERE, LocationType.SOMEWHERE);

        boolean changed;
        do {
            List<Constraint> workingSet = constraints.asList();
            for (Constraint c : workingSet)
                resolve(resolved, c);
            constraints.clear();
            changed = applyUpdates();
        } while (changed);

        for (Map.Entry<LocationTypeVar, LocationTypeVar> e : rewritten.entrySet()) {
            if (resolved.containsKey(e.getValue())) {
                resolved.put(e.getKey(), resolved.get(e.getValue()));
            }
        }

        for (LocationTypeVar v : allVars) {
            if (!resolved.containsKey(v)) {
                // Fallback to `SOMEWHERE`
                resolved.put(v, LocationType.SOMEWHERE);
            }
        }

        return resolved;
    }

    /**
     * Resolve one constrained
     * @param resolved - Already resolved vars and their type
     * @param c - The constraint to resolve
     */
    private void resolve(Map<LocationTypeVar, LocationType> resolved, Constraint c) {
        // Resolve equals
        if (c.isEq()) resolveEq(resolved, (Constraint.Eq) c);
            // Resolve <: Near, <: Far
        else if (c.isSub()) resolveSub(resolved, (Constraint.Sub) c);
        else if (c.isAdapt()) resolveAdapt(resolved, (Constraint.Adapt) c);
    }

    /**
     * Resolve one eq constrained
     * @param resolved - Already resolved vars and their type
     * @param eq - The constraint to resolve
     */
    private void resolveEq(Map<LocationTypeVar, LocationType> resolved, Constraint.Eq eq) {
        LocationTypeVar expected = getRewritten(eq.expected);
        LocationTypeVar actual = getRewritten(eq.actual);
        ASTNode<?> node = eq.node;

        if (expected == actual) return;

        if (expected.isLocationType() && actual.isLocationType()) {
            errors.add(
                new LocationTypeInferException(
                    new TypeError(node,
                        ErrorMessage.LOCATION_TYPE_CANNOT_ASSIGN, expected.toString(), actual.toString())));
            return;
        }

        if (expected.isLocationType()) {
            rewrite(actual, expected);
            resolved.put(actual, expected.asLocationType());
            return;
        }

        if (actual.isLocationType()) {
            rewrite(expected, actual);
            resolved.put(expected, actual.asLocationType());
            return;
        }

        rewrite(expected, actual);
    }

    /**
     * Resolve one sub constrained
     * @param resolved - Already resolved vars and their type
     * @param sub - The constraint to resolve
     */
    private void resolveSub(Map<LocationTypeVar, LocationType> resolved, Constraint.Sub sub) {
        LocationTypeVar expected = getRewritten(sub.expected);
        LocationTypeVar actual = getRewritten(sub.actual);
        ASTNode<?> node = sub.node;

        if (expected == actual) {
            return;
        }

        if (expected.isLocationType() && actual.isLocationType()) {
            if (!expected.asLocationType().isSubtypeOf(actual.asLocationType())) {
                errors.add(
                    new LocationTypeInferException(
                        new TypeError(node, ErrorMessage.LOCATION_TYPE_CANNOT_ASSIGN, expected.toString(), actual.toString())
                    ));
            }
            return;
        }

        if (actual == BOTTOM) {
            throw new IllegalArgumentException("Cannot resolve constraint " + expected + " <: BOTTOM");
        }

        if (actual == NEAR) {
            add(Constraint.eq(expected, NEAR, node));
            return;
        }

        if (actual == FAR) {
            add(Constraint.eq(expected, FAR, node));
            return;
        }

        if (actual == SOMEWHERE) {
            // Useless
            return;
        }

        if (expected == SOMEWHERE) {
            add(Constraint.eq(actual, SOMEWHERE, node));
            return;
        }

        if (expected == BOTTOM) {
            // Useless
            return;
        }

        List<Constraint> expectedCs = constraints.get(expected);
        List<Constraint> actualCs = constraints.get(actual);

        if (expectedCs.size() <= 1 && actualCs.size() <= 1) {
            add(Constraint.eq(expected, actual, node));
            return;
        }

        if (actualCs.stream().filter(c -> c.isSub() && ((Constraint.Sub) c).actual == actual).count() == 1) {
            add(Constraint.eq(expected, actual, node));
            return;
        }

        if (expected == NEAR
            && actualCs.stream().anyMatch(c -> c.isSub() && ((Constraint.Sub) c).expected == FAR)) {
            add(Constraint.eq(actual, SOMEWHERE, node));
            return;
        }

        // Search for reverse (actual <: expected)
        // Search in smaller list
        List<Constraint> toSearch = expectedCs.size() < actualCs.size() ? expectedCs : actualCs;
        if (toSearch.stream().anyMatch(c -> c.isSub()
            && ((Constraint.Sub) c).expected == actual
            && ((Constraint.Sub) c).actual == expected)) {
            add(Constraint.eq(expected, actual, node));
            return;
        }

        keep(sub);
    }

    /**
     * Resolve one adapt constrained
     * @param resolved - Already resolved vars and their type
     * @param adapt - The constraint to resolve
     */
    private void resolveAdapt(Map<LocationTypeVar, LocationType> resolved, Constraint.Adapt adapt) {
        LocationTypeVar expected = getRewritten(adapt.expected);
        LocationTypeVar actual = getRewritten(adapt.actual);
        AdaptDirection dir = adapt.dir;
        LocationTypeVar adaptTo = getRewritten(adapt.adaptTo);
        ASTNode<?> node = adapt.node;

        if (!resolved.containsKey(adaptTo)) {
            keep(adapt);
            return;
        }

        if (adaptTo == NEAR) {
            add(Constraint.eq(expected, actual, node));
            return;
        }

        if (adaptTo == SOMEWHERE) {
            add(Constraint.eq(expected, SOMEWHERE, node));
            return;
        }

        if (adaptTo == FAR) {
            LocationTypeVar res;
            if (actual == NEAR) {
                res = FAR;
            } else if (actual == SOMEWHERE) {
                res = SOMEWHERE;
            } else if (actual == BOTTOM) {
                errors.add(new LocationTypeInferException(
                    new TypeError(node, ErrorMessage.LOCATION_TYPE_CALL_ON_BOTTOM, new String[0])
                ));
                return;
            } else if (actual == FAR) {
                res = SOMEWHERE;
            } else {
                keep(adapt);
                return;
            }
            add(Constraint.eq(expected, res, node));
            return;
        }

        keep(adapt);
    }

    /**
     * Get the variable we should use for `lv` as it may have been rewritten
     * @param lv - The var to look up
     * @return - The variable to use in `lv`s place
     */
    private LocationTypeVar getRewritten(LocationTypeVar lv) {
        return rewritten.getOrDefault(lv, lv);
    }

    /**
     * Rewrite all constraints to use `to` rather than `from`. Store this information
     * @param from - The old var
     * @param to - The var to use
     */
    private void rewrite(LocationTypeVar from, LocationTypeVar to) {
        if (debug)
            System.out.println((char) 27 + "[34m" + "Rewriting " + from + " => " + to + (char) 27 + "[0m");

        if (rewritten.containsKey(from))
            throw new IllegalArgumentException("LocationVar " + from + " cannot be rewritten to " + to + "! Already rewritten to " + rewritten.get(from));

        if (rewritten.containsValue(from)) {
            for (Map.Entry<LocationTypeVar, LocationTypeVar> e : rewritten.entrySet()) {
                if (e.getValue().equals(from)) {
                    rewritten.put(e.getKey(), to);
                }
            }
        }
        rewritten.put(from, to);
    }

    /**
     * Apply all updates we have collected this iteration
     * @return - Whether any changes have occurred
     */
    private boolean applyUpdates() {
        boolean changed = false;
        for (Update u : updates) {
            Constraint c = u.constraint;

            for (LocationTypeVar v : u.constraint.vars())
                if (rewritten.containsKey(v))
                    c = c.replace(v, rewritten.get(v));

            switch (u.kind) {
                case ADD:
                    changed = true;
                    constraints.add(c);
                    break;
                case REMOVE:
                    changed = true;
                    constraints.remove(c);
                    break;
                case KEEP:
                    constraints.add(c);
            }
        }
        updates.clear();
        return changed;
    }

    private void add(Constraint c) {
        updates.add(new Update(UpdateKind.ADD, c));
    }

    private void remove(Constraint c) {
        updates.add(new Update(UpdateKind.REMOVE, c));
    }

    private void keep(Constraint c) {
        updates.add(new Update(UpdateKind.KEEP, c));
    }

    private enum UpdateKind {
        ADD, REMOVE, KEEP
    }

    /**
     * Used to track changes
     */
    private static class Update {
        public UpdateKind kind;
        public Constraint constraint;

        public Update(UpdateKind kind, Constraint c) {
            this.kind = kind;
            this.constraint = c;
        }
    }
}
