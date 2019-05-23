/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.frontend.typechecker;

import org.abs_models.frontend.ast.AnyTypeUse;

public final class AnyType extends Type {
    public static final AnyType INSTANCE = new AnyType();

    @Override
    public Type copy() {
        return INSTANCE;
    }

    private AnyType() {
    }

    @Override
    public boolean equals(Object o) {
        return o instanceof AnyType;
    }

    public int hashCode() {
        return 42;              // we're a singleton
    }

    @Override
    public boolean isAssignableTo(Type t) {
        return t instanceof AnyType; // TODO: Sane decision?
    }

    public boolean isAnyType() {
        return true;
    }

    @Override
    public String getSimpleName() {
        return "Any";
    }
    
    public AnyTypeUse toUse() {
        return new AnyTypeUse(getSimpleName(), new org.abs_models.frontend.ast.List<>());
    }
}
