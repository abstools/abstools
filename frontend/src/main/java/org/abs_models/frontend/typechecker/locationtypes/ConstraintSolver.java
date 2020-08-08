package org.abs_models.frontend.typechecker.locationtypes;

import org.abs_models.frontend.ast.Const;

import java.util.*;
import java.util.stream.Collectors;

public class ConstraintSolver {
    private final Constraints cs;
    private final List<LocationTypeInferException> errors = new ArrayList<>();
    private final Map<LocationTypeVar, LocationTypeVar> rewritten = new HashMap<>();

    private boolean changed = true;

    private ConstraintSolver(Constraints cs) {
        this.cs = cs;
    }

    private Map<LocationTypeVar, List<Constraint>> map() {
        return cs.getMap();
    }

    private List<LocationTypeVar> vars() {
        return new ArrayList<>(map().keySet());
    }

    private Set<Constraint> constraints() {
        return cs.getConstraints();
    }

    private Map<LocationTypeVar, LocationType> computeResults() {
        Map<LocationTypeVar, LocationType> resolved = new HashMap<>();
        resolved.put(LocationTypeVar.BOTTOM, LocationType.BOTTOM);
        resolved.put(LocationTypeVar.NEAR, LocationType.NEAR);
        resolved.put(LocationTypeVar.FAR, LocationType.FAR);
        resolved.put(LocationTypeVar.SOMEWHERE, LocationType.SOMEWHERE);

        while (changed) {
            changed = false;
            resolve(resolved);
        }

        for (Map.Entry<LocationTypeVar, LocationTypeVar> e : rewritten.entrySet()) {
            if (resolved.containsKey(e.getValue())) {
                resolved.put(e.getKey(), resolved.get(e.getValue()));
            }
        }

        return resolved;
    }

    private void resolve(Map<LocationTypeVar, LocationType> resolved) {
        // Resolve equals
        resolveEqs(resolved);

        // Resolve <: Near, <: Far
        resolveSubConsts(resolved);
    }

    private void resolveEqs(Map<LocationTypeVar, LocationType> resolved) {
        List<Constraint> eqs = constraints().stream().filter(Constraint::isEq).collect(Collectors.toList());
        for (Constraint c : eqs) {
            Constraint.Eq e = (Constraint.Eq) c;
            LocationTypeVar expected = rewritten.getOrDefault(e.expected, e.expected);
            LocationTypeVar actual = rewritten.getOrDefault(e.actual, e.actual);

            if (expected == actual) {
                cs.remove(c);
                continue;
            }

            if (resolved.containsKey(expected) && resolved.containsKey(actual)) {
                LocationType et = resolved.get(expected);
                LocationType at = resolved.get(actual);

                if (!et.equals(at)) {
                    errors.add(new LocationTypeInferException());
                    continue;
                }
            } else if (resolved.containsKey(expected)) {
                LocationType t = resolved.get(expected);
                resolved.put(actual, t);
            } else if (resolved.containsKey(actual)) {
                LocationType t = resolved.get(actual);
                resolved.put(expected, t);
            }

            // Propagate equation
            rewrite(expected, actual);

            cs.remove(c);
            changed = true;
        }
    }

    private void resolveSubConsts(Map<LocationTypeVar, LocationType> resolved) {
        List<Constraint.Sub> subs = constraints()
            .stream()
            .filter(Constraint::isSub)
            .map(c -> (Constraint.Sub) c)
            .collect(Collectors.toList());
        for (Constraint.Sub s : subs) {
            LocationTypeVar expected = s.expected;
            LocationTypeVar actual = s.actual;

            if (expected.isLocationType() && actual.isLocationType()) {
                if (!expected.asLocationType().isSubtypeOf(actual.asLocationType())) {
                    errors.add(new LocationTypeInferException());
                }
                cs.remove(s);
                changed = true;
                continue;
            }

            if (actual == LocationTypeVar.BOTTOM) {
                // This should never happen
                throw new IllegalArgumentException(/*TODO*/);
            }

            // Try to get a reverse sub
            Optional<Constraint.Sub> rev = subs
                .stream()
                .filter(sub -> sub.expected == actual && sub.actual == expected)
                .findFirst();
            if (rev.isPresent()) {
                cs.remove(s);
                cs.remove(rev.get());
                cs.add(Constraint.eq(expected, actual), null);
                changed = true;
                continue;
            }

            // if NEAR <: v && FAR <: v => v == SOMEWHERE
            if (expected == LocationTypeVar.NEAR || expected == LocationTypeVar.FAR) {
                boolean near = expected == LocationTypeVar.NEAR;
                Optional<Constraint.Sub> o = subs
                    .stream()
                    .filter(sub -> sub.expected == (near ? LocationTypeVar.FAR : LocationTypeVar.NEAR) && sub.actual == actual)
                    .findFirst();
                if (o.isPresent()) {
                    cs.remove(s);
                    cs.remove(o.get());
                    cs.add(Constraint.eq(LocationTypeVar.SOMEWHERE, actual), null);
                    changed = true;
                }
                continue;
            }
        }
        for (LocationTypeVar v : vars()) {
            if (resolved.containsKey(v)) continue;
            List<Constraint> constraints = map().get(v);
            if (constraints.size() == 1) {
                // Only one usage. Use it
                Constraint c = constraints.get(0);
                if (c.isSub()) {
                    Constraint.Sub s = (Constraint.Sub) c;
                    cs.remove(c);
                    if (s.expected.isLocationType()) {
                        resolved.put(s.actual, s.expected.asLocationType());
                        changed = true;
                    } else if (s.actual.isLocationType()) {
                        resolved.put(s.expected, s.actual.asLocationType());
                        changed = true;
                    } else {
                        cs.add(Constraint.eq(s.expected, s.actual), null);
                        changed = true;
                    }
                } else if (c.isEq()) {
                    // Should already be resolved
                    assert false;
                } else {
                    // TODO

                }
            } else {
                List<Constraint.Sub> sc = constraints
                    .stream()
                    .filter(Constraint::isSub)
                    .map(c -> (Constraint.Sub) c)
                    .filter(s -> s.expected == v)
                    .collect(Collectors.toList());
                if (sc.size() == 1) {
                    Constraint.Sub c = sc.get(0);
                    cs.remove(c);
                    cs.add(Constraint.eq(v, c.actual), null);
                    changed = true;
                }
            }
        }
    }

    private void rewrite(LocationTypeVar from, LocationTypeVar to) {
        System.out.println((char) 27 + "[34m" + "Rewriting " + from + " => " + to + (char) 27 + "[0m");

        Set<Constraint> toAdd = new HashSet<>();
        Set<Constraint> toRemove = new HashSet<>();
        for (Constraint c : constraints()) {
            if (c.contains(from)) {
                toRemove.add(c);
                Constraint nc = c.replace(from, to);
                toAdd.add(nc);
            }
        }
        for (Constraint c : toAdd) {
            cs.add(c, null);
        }
        for (Constraint c : toRemove) {
            cs.remove(c);
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

    public static Map<LocationTypeVar, LocationType> solve(Constraints cs) {
        return new ConstraintSolver(cs).computeResults();
    }
}
