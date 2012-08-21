/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.lib.runtime;

import abs.backend.java.lib.types.ABSValue;

public class ABSField {

    // to be overridden
    public ABSValue init(ABSDynamicObject t) {
        return null;
    }
}
