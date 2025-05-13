/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.lib.expr;

import org.abs_models.backend.java.lib.types.ABSAlgebraicDataType;
import org.abs_models.backend.java.lib.types.ABSBuiltInDataType;

public class PatternConstructor extends Pattern {
    public final Pattern[] subpattern;
    public final Class<?> constructorClass;

    public PatternConstructor(Class<?> constructorClass, Pattern... subpattern) {
        this.constructorClass = constructorClass;
        this.subpattern = subpattern;
    }

    @Override
    public boolean match(Object value, PatternBinding b) {
        switch (value) {
            case java.lang.String s:
                return constructorClass.getSimpleName().equals("String");
            case ABSBuiltInDataType bt:
                return constructorClass.getSimpleName().endsWith(bt.getConstructorName());
            case ABSAlgebraicDataType dt:
                if (!constructorClass.equals(dt.getClass()))
                    return false;
                else {
                    for (int i = 0; i < dt.getNumArgs(); i++) {
                        if (!subpattern[i].match(dt.getArg(i), b)) return false;
                    }
                    return true;
                }
            default: return false;
        }
    }

}
