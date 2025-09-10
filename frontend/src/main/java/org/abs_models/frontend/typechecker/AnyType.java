/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.frontend.typechecker;

import org.abs_models.frontend.ast.TypeParameterDecl;
import org.abs_models.frontend.typechecker.nullable.NullableType;
import org.abs_models.frontend.typechecker.nullable.PrimitiveNullableType;
import com.google.common.collect.ImmutableMap;

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
        if (t.isAnyType())
            return true;

        return false;
    }

    public boolean isAnyType() {
        return true;
    }

    @Override
    public String getSimpleName() {
        return "Any";
    }

    @Override
    public NullableType instantiateNullableType(ImmutableMap<TypeParameterDecl, NullableType> inst) {
        return PrimitiveNullableType.NonApplicable;
    }
}
