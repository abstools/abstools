/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.lib.runtime;

import abs.backend.java.lib.types.ABSBool;
import abs.backend.java.lib.types.ABSRef;
import abs.backend.java.lib.types.ABSValue;

public abstract class ABSClosure implements ABSRef {
    
    // run the method
    public abstract ABSValue exec(ABSDynamicObject t, ABSValue... params);


    @Override
    public ABSBool eq(ABSValue o) {
        return ABSBool.fromBoolean(this == o);
    }

    @Override
    public ABSBool notEq(ABSValue o) {
        return eq(o).negate();
    }

    @Override
    public boolean isDataType() {
        return false;
    }

    @Override
    public boolean isReference() {
        return true;
    }
}
