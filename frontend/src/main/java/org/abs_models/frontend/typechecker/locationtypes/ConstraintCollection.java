package org.abs_models.frontend.typechecker.locationtypes;

import java.util.*;

/**
 * A class that collects constraints over variables and allows for efficient access to them
 */
public class ConstraintCollection {
    public static final Comparator<Constraint> comp = Comparator.comparing(Constraint::isEq)
        .thenComparing(Constraint::isSub).reversed();

    private final List<Constraint> constraints = new ArrayList<>();
    private final Map<LocationTypeVar, List<Constraint>> map = new HashMap<>();
    private final Set<Constraint> seen = new HashSet<>();
    private boolean debug = false;

    /**
     * Enables debug output, i.e., prints when a constraint is added or removed
     */
    public void enableDebug() {
        this.debug = true;
    }

    /**
     * Adds a constraint to the collection
     * @param c - The constraint to add
     */
    public void add(Constraint c) {
        if (seen.contains(c)) return;
        seen.add(c);
        if (debug)
            logConstraintAddition(c);
        constraints.add(c);
        for (LocationTypeVar v : c.vars()) {
            List<Constraint> cs = map.computeIfAbsent(v, k -> new ArrayList<>());

            cs.add(c);
        }
    }

    /**
     * Removes a constraint of the collection
     * @param c - The constraint to remove
     */
    public void remove(Constraint c) {
        seen.remove(c);
        constraints.remove(c);
        if (debug)
            logConstraintRemoval(c);
        for (LocationTypeVar v : c.vars()) {
            List<Constraint> cs = map.computeIfAbsent(v, k -> new ArrayList<>());

            cs.remove(c);
        }
    }

    /**
     * Gather all constraints as a list
     * @return All constraints as a list
     */
    public List<Constraint> asList() {
        return constraints;
    }

    /**
     * Gets all constraints for the variable
     * @param lv - The variable
     * @return - A list of all constraints for this variable. Always notNull
     */
    public List<Constraint> get(LocationTypeVar lv) {
        return map.getOrDefault(lv, new ArrayList<>());
    }

    /**
     * @return - Whether there is at least one constraint in the collection
     */
    public boolean hasNext() {
        return !constraints.isEmpty();
    }

    /**
     * Removes and returns one constraint
     * @return - The next constraint
     */
    public Constraint next() {
        Constraint c = constraints.remove(0);
        if (c == null) return null;

        for (LocationTypeVar v : c.vars()) {
            map.get(v).remove(c);
        }

        return c;
    }

    /**
     *
     * @return - All variables in the collection
     */
    public Set<LocationTypeVar> getVars() {
        return map.keySet();
    }

    /**
     *
     * @return - The size of the collection
     */
    public int size() {
        return constraints.size();
    }

    /**
     * Remove all constraints
     */
    public void clear() {
        constraints.clear();
        seen.clear();
        map.clear();
    }

    private void logConstraintAddition(Constraint c) {
        String at = c.node == null ? "" : " @" + c.node.getPositionString();
        System.out.println("Added constraint " + c + at);
    }

    private void logConstraintRemoval(Constraint c) {
        String at = c.node == null ? "" : " @" + c.node.getPositionString();
        System.out.println("Removed constraint " + c + at);
    }

    /**
     * Prints all constraints
     */
    public void print() {
        for (Constraint c : constraints) {
            System.out.println(c);
        }
    }
}
