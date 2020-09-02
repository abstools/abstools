package org.abs_models.frontend.typechecker.locationtypes;

import org.abs_models.frontend.analyser.ErrorMessage;
import org.abs_models.frontend.analyser.SemanticConditionList;
import org.abs_models.frontend.analyser.TypeError;
import org.abs_models.frontend.ast.ASTNode;
import org.abs_models.frontend.typechecker.ext.AdaptDirection;

import java.util.*;

import static org.abs_models.frontend.typechecker.locationtypes.LocationTypeVar.*;


public class ConstraintSolver {
    private final Constraints constraints;
    private final SemanticConditionList errors = new SemanticConditionList();
    private final Map<LocationTypeVar, LocationTypeVar> rewritten = new HashMap<>();

    private final List<Update> updates = new ArrayList<>();

    private boolean debug;

    public ConstraintSolver(Constraints cs, boolean debug) {
        this.constraints = cs;
        this.debug = debug;
    }

    public Map<LocationTypeVar, LocationType> solve() {
        Map<LocationTypeVar, LocationType> resolved = new HashMap<>();
        resolved.put(BOTTOM, LocationType.BOTTOM);
        resolved.put(NEAR, LocationType.NEAR);
        resolved.put(FAR, LocationType.FAR);
        resolved.put(SOMEWHERE, LocationType.SOMEWHERE);

        boolean changed;
        do {
            while (constraints.hasNext()) {
                resolve(resolved, constraints.next());
            }
            changed = applyUpdates();
        } while (changed);

        for (Map.Entry<LocationTypeVar, LocationTypeVar> e : rewritten.entrySet()) {
            if (resolved.containsKey(e.getValue())) {
                resolved.put(e.getKey(), resolved.get(e.getValue()));
            }
        }

        for (LocationTypeVar v : constraints.getVars()) {
            if (!resolved.containsKey(v)) {
                // Fallback to `SOMEWHERE`
                resolved.put(v, LocationType.SOMEWHERE);
            }
        }

        return resolved;
    }

    private void resolve(Map<LocationTypeVar, LocationType> resolved, Constraint c) {
        // Resolve equals
        if (c.isEq()) resolveEq(resolved, (Constraint.Eq) c);
            // Resolve <: Near, <: Far
        else if (c.isSub()) resolveSub(resolved, (Constraint.Sub) c);
        else if (c.isAdapt()) resolveAdapt(resolved, (Constraint.Adapt) c);
    }

    private void resolveEq(Map<LocationTypeVar, LocationType> resolved, Constraint.Eq eq) {
        LocationTypeVar expected = eq.expected;
        LocationTypeVar actual = eq.actual;
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

    private void resolveSub(Map<LocationTypeVar, LocationType> resolved, Constraint.Sub sub) {
        LocationTypeVar expected = sub.expected;
        LocationTypeVar actual = sub.actual;
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
            // TODO par far
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

        if (expectedCs.size() == 1 || actualCs.size() == 1) {
            add(Constraint.eq(expected, actual, node));
            return;
        }

        if (actualCs.stream().noneMatch(c -> c.isSub() && ((Constraint.Sub) c).actual == actual)) {
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

    private void resolveAdapt(Map<LocationTypeVar, LocationType> resolved, Constraint.Adapt adapt) {
        LocationTypeVar expected = adapt.expected;
        LocationTypeVar actual = adapt.actual;
        AdaptDirection dir = adapt.dir;
        LocationTypeVar adaptTo = adapt.adaptTo;
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
        }

        keep(adapt);
    }

    private void rewrite(LocationTypeVar from, LocationTypeVar to) {
        if (debug)
            System.out.println((char) 27 + "[34m" + "Rewriting " + from + " => " + to + (char) 27 + "[0m");

        for (Constraint c : constraints.get(from)) {
            add(c.replace(from, to));
            remove(c);
        }

        if (rewritten.containsValue(from)) {
            for (Map.Entry<LocationTypeVar, LocationTypeVar> e : rewritten.entrySet()) {
                if (e.getValue().equals(from)) {
                    rewritten.put(e.getKey(), to);
                }
            }
        }
        rewritten.put(from, to);
    }

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

    private static class Update {
        public UpdateKind kind;
        public Constraint constraint;

        public Update(UpdateKind kind, Constraint c) {
            this.kind = kind;
            this.constraint = c;
        }
    }
}
