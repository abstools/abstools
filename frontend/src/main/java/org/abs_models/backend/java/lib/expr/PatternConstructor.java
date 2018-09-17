/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.lib.expr;

import org.abs_models.backend.java.lib.types.ABSDataType;
import org.abs_models.backend.java.lib.types.ABSValue;

public class PatternConstructor extends Pattern {
    public final Pattern[] subpattern;
    public final Class<?> constructorClass;

    public PatternConstructor(Class<?> constructorClass, Pattern... subpattern) {
        this.constructorClass = constructorClass;
        this.subpattern = subpattern;
    }

    @Override
    public boolean match(ABSValue dt, PatternBinding b) {
        if (dt instanceof ABSDataType) {
            return ((ABSDataType) dt).match(this, b);
        } else {
            return false;
        }
    }

}
