/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.frontend.typechecker;

public final class BottomType extends Type {
    public static final BottomType INSTANCE = new BottomType();

    private BottomType() {
    }

    @Override
    public boolean equals(Object o) {
        return o instanceof BottomType;
    }

    public int hashCode() {
        return 1337;
    }

    @Override
    public boolean isBottomType() {
        return true;
    }

    @Override
    public boolean isAssignableTo(Type t) {
        return true; // Bottom type is assignable to everything (but it does not have any values)
    }

    public String toString() {
        return getSimpleName();
    }

    @Override
    public String getSimpleName() {
        return "<BOTTOM>";
    }

    @Override
    public Type copy() {
        return new BottomType();
    }
}
