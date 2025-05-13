/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.lib.expr;

public class PatternVariable extends Pattern {
    private final String varName;

    public PatternVariable(String name) {
        this.varName = name;
    }

    @Override
    public boolean match(Object dt, PatternBinding b) {
        b.addBinding(dt);
        return true;
    }

}
