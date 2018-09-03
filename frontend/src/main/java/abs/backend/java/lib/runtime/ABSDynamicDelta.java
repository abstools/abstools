/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.lib.runtime;

import abs.backend.java.lib.runtime.metaABS.Delta;

public class ABSDynamicDelta extends ABSDynamicObject {

    public ABSDynamicDelta() {
        super(Delta.singleton());
    }

    private String name;

    public String getName() {
        return name;
    }
    public void setName(String name) {
        this.name = name;
    }
    public void apply() {
        // TODO implement
        
    }
    
}
