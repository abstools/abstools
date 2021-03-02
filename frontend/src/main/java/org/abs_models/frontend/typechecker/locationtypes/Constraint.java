package org.abs_models.frontend.typechecker.locationtypes;

import org.abs_models.frontend.ast.ASTNode;
import org.abs_models.frontend.typechecker.ext.AdaptDirection;

import java.util.Objects;

/**
 * Represents a relationship of location type variables
 */
public abstract class Constraint {
    public final ASTNode<?> node;

    protected Constraint(ASTNode<?> node) {
        this.node = node;
    }

    /**
     * Determines whether the constraint contains `lv`
     * @param lv - The variable to test for
     * @return - Whether `lv` is in the constraint
     */
    abstract boolean contains(LocationTypeVar lv);

    /**
     *
     * @return - All variables in the constraint
     */
    abstract LocationTypeVar[] vars();

    /**
     *
     * @return Whether the constraint is a sub-constraint
     */
    public boolean isSub() {
        return false;
    }

    /**
     *
     * @return Whether the constraint is an eq-constraint
     */
    public boolean isEq() {
        return false;
    }

    /**
     *
     * @return Whether the constraint is an adapt-constraint
     */
    public boolean isAdapt() {
        return false;
    }

    /**
     * Replaces all occurrences of `from` with `to`
     */
    public abstract Constraint replace(LocationTypeVar from, LocationTypeVar to);

    /**
     * Creates a new constraint: `expected` <: `actual`
     * @param expected - The lhs of the constraint
     * @param actual - The rhs of the constraint
     * @param node - The corresponding node
     * @return - The new sub constraint
     */
    public static Constraint sub(LocationTypeVar expected, LocationTypeVar actual, ASTNode<?> node) {
        if (expected == null) {
            System.out.println("Expected is null in " + node);
            throw new RuntimeException();
        }
        return new Sub(expected, actual, node);
    }

    /**
     * Creates a new constraint: `expected` = `actual`
     * @param expected - The lhs of the constraint
     * @param actual - The rhs of the constraint
     * @param node - The corresponding node
     * @return - The new eq constraint
     */
    public static Constraint eq(LocationTypeVar expected, LocationTypeVar actual, ASTNode<?> node) {
        return new Eq(expected, actual, node);
    }

    /**
     * Creates a new constraint: `expected` = `actual` |> `adaptTo`
     * @param expected - The lhs of the constraint
     * @param actual - The rhs of the constraint
     * @param dir - The adaption direction
     * @param adaptTo - The variable to adapt to
     * @param node - The corresponding node
     * @return - The new adapt constraint
     */
    public static Constraint adapt(LocationTypeVar expected, LocationTypeVar actual, AdaptDirection dir, LocationTypeVar adaptTo, ASTNode<?> node) {
        return new Adapt(expected, actual, dir, adaptTo, node);
    }

    // expected <: actual
    public static class Sub extends Constraint {
        public final LocationTypeVar expected;
        public final LocationTypeVar actual;

        public Sub(LocationTypeVar expected, LocationTypeVar actual, ASTNode<?> node) {
            super(node);
            this.expected = expected;
            this.actual = actual;
        }

        @Override
        public boolean isSub() {
            return true;
        }

        @Override
        public Constraint replace(LocationTypeVar from, LocationTypeVar to) {
            LocationTypeVar expected = this.expected == from ? to : this.expected;
            LocationTypeVar actual = this.actual == from ? to : this.actual;
            return sub(expected, actual, node);
        }

        @Override
        boolean contains(LocationTypeVar lv) {
            return expected == lv || actual == lv;
        }

        @Override
        LocationTypeVar[] vars() {
            return new LocationTypeVar[]{expected, actual};
        }

        @Override
        public String toString() {
            return "" + expected + " <: " + actual;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            Sub sub = (Sub) o;
            return expected.equals(sub.expected) &&
                actual.equals(sub.actual);
        }

        @Override
        public int hashCode() {
            return Objects.hash(expected, actual);
        }
    }

    // expected = actual
    public static class Eq extends Constraint {
        public final LocationTypeVar expected;
        public final LocationTypeVar actual;

        public Eq(LocationTypeVar expected, LocationTypeVar actual, ASTNode<?> node) {
            super(node);
            this.expected = expected;
            this.actual = actual;
        }

        @Override
        public boolean isEq() {
            return true;
        }

        @Override
        public Constraint replace(LocationTypeVar from, LocationTypeVar to) {
            LocationTypeVar expected = this.expected == from ? to : this.expected;
            LocationTypeVar actual = this.actual == from ? to : this.actual;
            return eq(expected, actual, node);
        }

        @Override
        boolean contains(LocationTypeVar lv) {
            return expected == lv || actual == lv;
        }

        @Override
        LocationTypeVar[] vars() {
            return new LocationTypeVar[]{expected, actual};
        }

        @Override
        public String toString() {
            return "" + expected + " = " + actual;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            Eq eq = (Eq) o;
            return expected.equals(eq.expected) &&
                actual.equals(eq.actual);
        }

        @Override
        public int hashCode() {
            return Objects.hash(expected, actual);
        }
    }

    // expected = actual |> adaptTo, used for calls
    public static class Adapt extends Constraint {
        public final LocationTypeVar expected;
        public final LocationTypeVar actual;
        public final LocationTypeVar adaptTo;
        public final AdaptDirection dir;

        public Adapt(LocationTypeVar expected, LocationTypeVar actual, AdaptDirection dir, LocationTypeVar adaptTo, ASTNode<?> node) {
            super(node);
            this.expected = expected;
            this.actual = actual;
            this.adaptTo = adaptTo;
            this.dir = dir;
        }

        @Override
        public boolean isAdapt() {
            return true;
        }

        @Override
        public Constraint replace(LocationTypeVar from, LocationTypeVar to) {
            LocationTypeVar expected = this.expected == from ? to : this.expected;
            LocationTypeVar actual = this.actual == from ? to : this.actual;
            LocationTypeVar adaptTo = this.adaptTo == from ? to : this.adaptTo;
            return adapt(expected, actual, dir, adaptTo, node);
        }

        @Override
        boolean contains(LocationTypeVar lv) {
            return lv == expected || lv == actual || lv == adaptTo;
        }

        @Override
        LocationTypeVar[] vars() {
            return new LocationTypeVar[]{expected, actual, adaptTo};
        }

        @Override
        public String toString() {
            return "" + expected + " = " + actual + " |> " + adaptTo;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            Adapt adapt = (Adapt) o;
            return expected.equals(adapt.expected) &&
                actual.equals(adapt.actual) &&
                Objects.equals(adaptTo, adapt.adaptTo) &&
                dir == adapt.dir;
        }

        @Override
        public int hashCode() {
            return Objects.hash(expected, actual, adaptTo, dir);
        }
    }
}
