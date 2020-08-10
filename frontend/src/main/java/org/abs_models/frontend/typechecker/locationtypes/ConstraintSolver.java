package org.abs_models.frontend.typechecker.locationtypes;

import org.abs_models.frontend.ast.Const;
import org.abs_models.frontend.typechecker.ext.AdaptDirection;

import java.util.*;
import java.util.stream.Collectors;

import static org.abs_models.frontend.typechecker.locationtypes.LocationTypeVar.*;


public class ConstraintSolver {
    private final Constraints constraints;
    private final List<LocationTypeInferException> errors = new ArrayList<>();
    private final Map<LocationTypeVar, LocationTypeVar> rewritten = new HashMap<>();

    private final List<Update> updates = new ArrayList<>();

    private ConstraintSolver(Constraints cs) {
        this.constraints = cs;
    }

    private Map<LocationTypeVar, LocationType> computeResults() {
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
        if (expected == actual) return;

        if (expected.isLocationType() && actual.isLocationType()) {
            errors.add(new LocationTypeInferException());
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

        if (expected == actual) {
            return;
        }

        if (expected.isLocationType() && actual.isLocationType()) {
            if (!expected.asLocationType().isSubtypeOf(actual.asLocationType())) {
                errors.add(new LocationTypeInferException());
            }
            return;
        }

        if (actual == BOTTOM) {
            errors.add(new LocationTypeInferException());
            return;
        }

        if (actual == NEAR) {
            add(Constraint.eq(expected, NEAR));
            return;
        }

        if (actual == FAR) {
            // TODO par far
            add(Constraint.eq(expected, FAR));
            return;
        }

        if (actual == SOMEWHERE) {
            // Useless
            return;
        }

        if (expected == SOMEWHERE) {
            add(Constraint.eq(actual, SOMEWHERE));
            return;
        }

        if (expected == BOTTOM) {
            // Useless
            return;
        }

        List<Constraint> expectedCs = constraints.get(expected);
        List<Constraint> actualCs = constraints.get(actual);

        if (expectedCs.size() == 1 || actualCs.size() == 1) {
            add(Constraint.eq(expected, actual));
            return;
        }

        if (actualCs.stream().noneMatch(c -> c.isSub() && ((Constraint.Sub) c).actual == actual)) {
            add(Constraint.eq(expected, actual));
            return;
        }

        if (expected == NEAR
            && actualCs.stream().anyMatch(c -> c.isSub() && ((Constraint.Sub) c).expected == FAR)) {
            add(Constraint.eq(actual, SOMEWHERE));
            return;
        }

        keep(sub);
    }

    private void resolveAdapt(Map<LocationTypeVar, LocationType> resolved, Constraint.Adapt adapt) {
        LocationTypeVar expected = adapt.expected;
        LocationTypeVar actual = adapt.actual;
        AdaptDirection dir = adapt.dir;
        LocationTypeVar adaptTo = adapt.adaptTo;

        if (!resolved.containsKey(adaptTo)) {
            keep(adapt);
            return;
        }

        if (adaptTo == NEAR) {
            add(Constraint.eq(expected, actual));
            return;
        }

        if (adaptTo == SOMEWHERE) {
            add(Constraint.eq(expected, SOMEWHERE));
            return;
        }

        if (adaptTo == FAR) {
            LocationTypeVar res;
            if (actual == NEAR) {
                res = FAR;
            } else if (actual == SOMEWHERE) {
                res = SOMEWHERE;
            } else if (actual == BOTTOM) {
                errors.add(new LocationTypeInferException());
                return;
            } else if (actual == FAR) {
                res = SOMEWHERE;
            } else {
                keep(adapt);
                return;
            }
            add(Constraint.eq(expected, res));
        }

        keep(adapt);
    }

    private void rewrite(LocationTypeVar from, LocationTypeVar to) {
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
            switch (u.kind) {
                case ADD:
                    changed = true;
                    constraints.add(u.constraint, null);
                    break;
                case REMOVE:
                    changed = true;
                    constraints.remove(u.constraint);
                    break;
                case KEEP:
                    constraints.add(u.constraint, null);
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

    public static Map<LocationTypeVar, LocationType> solve(Constraints cs) {
        return new ConstraintSolver(cs).computeResults();
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
