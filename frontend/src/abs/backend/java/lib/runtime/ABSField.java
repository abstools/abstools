/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.lib.runtime;

import abs.backend.java.lib.types.ABSValue;

public class ABSField {

    // Not sure if all this is useful.
    // ABSFields are created upon creation of classes (ABSDynamicClass);
    // However, the initial value of a field might not be know until instances of that class are created (i.e. ABSDynamicObjects)
    
    private ABSValue intialValue;

    public ABSField() {
        super();
    }

    public ABSField(ABSValue v) {
        super();
        this.intialValue = v;
    }
    
    public void setInitialValue(ABSValue v) {
        intialValue = v;
    }
    public ABSValue getInitialValue() {
        return intialValue;
    }

}
