/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.frontend.typechecker;

import org.abs_models.frontend.ast.TypeParameterDecl;
import org.abs_models.frontend.typechecker.nullable.NullCheckerExtension;
import org.abs_models.frontend.typechecker.nullable.NullableType;
import com.google.common.collect.ImmutableMap;

public abstract class ReferenceType extends Type {
    
    @Override
    public boolean isReferenceType() {
        return true;
    }

    @Override
    public NullableType instantiateNullableType(ImmutableMap<TypeParameterDecl, NullableType> inst) {
        return NullCheckerExtension.getNullableType(this);
    }
}
