package org.abs_models.frontend.typechecker.locationtypes;

import org.abs_models.frontend.ast.ASTNode;
import org.abs_models.frontend.typechecker.ext.AdaptDirection;

import java.util.Objects;

public abstract class Constraint {
    public final ASTNode<?> node;

    protected Constraint(ASTNode<?> node) {
        this.node = node;
    }

    abstract boolean contains(LocationTypeVar lv);

    abstract LocationTypeVar[] vars();

    public boolean isSub() {
        return false;
    }

    public boolean isEq() {
        return false;
    }

    public boolean isAdapt() {
        return false;
    }

    public abstract Constraint replace(LocationTypeVar from, LocationTypeVar to);

    public static Constraint sub(LocationTypeVar expected, LocationTypeVar actual, ASTNode<?> node) {
        return new Sub(expected, actual, node);
    }

    public static Constraint eq(LocationTypeVar expected, LocationTypeVar actual, ASTNode<?> node) {
        return new Eq(expected, actual, node);
    }

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
