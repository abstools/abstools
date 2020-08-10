package org.abs_models.frontend.typechecker.locationtypes;

import org.abs_models.frontend.ast.ASTNode;
import org.abs_models.frontend.ast.Const;

import java.util.*;
import java.util.stream.Collectors;

public class Constraints {
    public static final Comparator<Constraint> comp = Comparator.comparing(Constraint::isEq)
        .thenComparing(Constraint::isSub).reversed();

    private final List<Constraint> constraints = new ArrayList<>();
    private final Map<LocationTypeVar, List<Constraint>> map = new HashMap<>();
    private final Set<Constraint> seen = new HashSet<>();

    public void add(Constraint c, ASTNode<?> origin) {
        if (seen.contains(c)) return;
        seen.add(c);
        logConstraint(c, origin);
        constraints.add(c);
        for (LocationTypeVar v : c.vars()) {
            List<Constraint> cs = map.computeIfAbsent(v, k -> new ArrayList<>());

            cs.add(c);
        }
    }

    public void remove(Constraint c) {
        constraints.remove(c);
        for (LocationTypeVar v : c.vars()) {
            List<Constraint> cs = map.computeIfAbsent(v, k -> new ArrayList<>());

            cs.remove(c);
        }
    }

    public List<Constraint> get(LocationTypeVar lv) {
        return map.getOrDefault(lv, new ArrayList<>());
    }

    public boolean hasNext() {
        return !constraints.isEmpty();
    }

    public Constraint next() {
        Constraint c = constraints.get(0);
        if (c == null) return null;

        for (LocationTypeVar v : c.vars()) {
            map.get(v).remove(c);
        }

        return c;
    }

    public int size() {
        return constraints.size();
    }

    private void logConstraint(Constraint c, ASTNode<?> origin) {
        boolean show = false;
        for (LocationTypeVar v : c.vars())
            if (v.getId() > 51 || v.isLocationType()) {
                show = true;
                break;
            }
        String at = origin == null ? "" : " @" + origin.getPositionString();
        if (show)
            System.out.println("Add constraint " + c + at);
    }

    public void print() {
        for (Constraint c : constraints) {
            System.out.println(c);
        }
    }
}
