/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.frontend.typechecker;

public final class SuperType extends Type {
    public static final SuperType INSTANCE = new SuperType();

    @Override
    public Type copy() {
        return INSTANCE;
    }

    private SuperType() {
    }

    public boolean isSuperType() {
        return true;
    }

    @Override
    public boolean equals(Object o) {
        return o instanceof SuperType;
    }

    public int hashCode() {
        return 42;              // we're a singleton
    }

    @Override
    public boolean isAssignableTo(Type t) {
        return t instanceof SuperType;
    }

    @Override
    public String getSimpleName() {
        return "Super";
    }
}
