/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.lib.runtime;

import abs.backend.java.lib.types.ABSValue;

public abstract class ABSClosure {
    
    // run the method
    public abstract ABSValue exec(ABSDynamicObject t, ABSValue... params);
}
