/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.lib.runtime.metaABS;

import org.abs_models.backend.java.lib.runtime.ABSClosure;
import org.abs_models.backend.java.lib.runtime.ABSDynamicClass;
import org.abs_models.backend.java.lib.runtime.ABSDynamicDelta;
import org.abs_models.backend.java.lib.runtime.ABSDynamicObject;
import org.abs_models.backend.java.lib.types.ABSUnit;

public class Delta {
    private static ABSDynamicClass thisClass;

    /* 
     * Create singleton object
     */
    public static ABSDynamicClass singleton() {
        if (thisClass == null) {
            thisClass = new ABSDynamicClass();
            setupMetaAPI();
        }
        return thisClass;
    }

    /*
     * Define the methods of this class
     */
    public static void setupMetaAPI() {
        thisClass.setName("Delta");

        thisClass.addMethod("getName", new ABSClosure() {
            @Override
            public String exec(ABSDynamicObject t, Object... params) {
                ABSDynamicDelta delta = (ABSDynamicDelta)t;
                return delta.getName();
            }
        });

        thisClass.addMethod(/*ABSUnit*/ "apply", new ABSClosure() {
            @Override
            public Object exec(ABSDynamicObject t, Object... params) {
                ABSDynamicDelta delta = (ABSDynamicDelta)t;
                delta.apply();
                return ABSUnit.UNIT;
            }
        });
        
    }
}
