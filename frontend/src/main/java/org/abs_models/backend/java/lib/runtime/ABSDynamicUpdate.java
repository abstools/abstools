/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.lib.runtime;

import org.abs_models.backend.java.lib.runtime.metaABS.Update;

public class ABSDynamicUpdate extends ABSDynamicObject {

    public ABSDynamicUpdate() {
        super(Update.singleton());
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
