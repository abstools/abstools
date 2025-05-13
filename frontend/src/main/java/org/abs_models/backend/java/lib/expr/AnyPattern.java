/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.lib.expr;

public class AnyPattern extends Pattern {

    @Override
    public boolean match(Object dt, PatternBinding binding) {
        return true;
    }

}
