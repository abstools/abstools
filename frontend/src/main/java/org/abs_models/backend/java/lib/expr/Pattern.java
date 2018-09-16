/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.lib.expr;

import org.abs_models.backend.java.lib.types.ABSValue;

public abstract class Pattern {
    public PatternBinding match(ABSValue dt) {
        PatternBinding b = new PatternBinding();
        if (match(dt, b))
            return b;
        else
            return null;
    }

    public abstract boolean match(ABSValue dt, PatternBinding binding);

}
