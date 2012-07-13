/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.lib.expr;

import abs.backend.java.lib.types.ABSValue;

public class AnyPattern extends Pattern {

    @Override
    public boolean match(ABSValue dt, PatternBinding binding) {
        return true;
    }

}
