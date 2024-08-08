/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.lib.types;

import org.abs_models.backend.java.lib.expr.PatternBinding;
import org.abs_models.backend.java.lib.expr.PatternConstructor;

public abstract class ABSBuiltInDataType extends ABSDataType {
    public final String constructorName;

    protected ABSBuiltInDataType(String constructorName) {
        this.constructorName = constructorName;
    }

    public String getConstructorName() {
        return constructorName;
    }

    @Override
    public boolean match(PatternConstructor p, PatternBinding b) {
        if (p.constructorClass.getSimpleName().endsWith(constructorName)) {
            return true;
        } else {
            return false;
        }
    }

}
