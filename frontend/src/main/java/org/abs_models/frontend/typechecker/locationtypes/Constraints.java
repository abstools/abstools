package org.abs_models.frontend.typechecker.locationtypes;

import org.abs_models.frontend.ast.ASTNode;

import java.util.*;
import java.util.stream.Collectors;

public class Constraints {
    private final Set<Constraint> constraints = new HashSet<>();
    private final Map<LocationTypeVar, List<Constraint>> map = new HashMap<>();

    public void add(Constraint c, ASTNode<?> origin) {
        logConstraint(c, origin);
        constraints.add(c);
        for (LocationTypeVar v : c.vars()) {
            List<Constraint> cs = map.computeIfAbsent(v, k -> new ArrayList<>());

            cs.add(c);
        }
    }

    public List<Constraint> get(LocationTypeVar lv) {
        return map.getOrDefault(lv, new ArrayList<>());
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

    public Set<Constraint> getConstraints() {
        return constraints;
    }

    public Map<LocationTypeVar, List<Constraint>> getMap() {
        return map;
    }
}
