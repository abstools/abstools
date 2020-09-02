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
    private boolean debug = false;

    public void enableDebug() {
        this.debug = true;
    }

    public void add(Constraint c) {
        if (seen.contains(c)) return;
        seen.add(c);
        if (debug)
            logConstraint(c);
        constraints.add(c);
        for (LocationTypeVar v : c.vars()) {
            List<Constraint> cs = map.computeIfAbsent(v, k -> new ArrayList<>());

            cs.add(c);
        }
    }

    public void remove(Constraint c) {
        seen.remove(c);
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
        Constraint c = constraints.remove(0);
        if (c == null) return null;

        for (LocationTypeVar v : c.vars()) {
            map.get(v).remove(c);
        }

        return c;
    }

    public Set<LocationTypeVar> getVars() {
        return map.keySet();
    }

    public int size() {
        return constraints.size();
    }

    private void logConstraint(Constraint c) {
        String at = c.node == null ? "" : " @" + c.node.getPositionString();
        System.out.println("Add constraint " + c + at);
    }

    public void print() {
        for (Constraint c : constraints) {
            System.out.println(c);
        }
    }
}
