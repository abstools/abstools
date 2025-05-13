/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.lib.runtime.metaABS;

import org.abs_models.backend.java.lib.runtime.ABSClosure;
import org.abs_models.backend.java.lib.runtime.ABSDynamicClass;
import org.abs_models.backend.java.lib.runtime.ABSDynamicObject;
import org.abs_models.backend.java.lib.types.ABSUnit;

public class Clazz {
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
        thisClass.setName("Class");

        thisClass.addMethod("getName", new ABSClosure() {
            @Override
            public String exec(ABSDynamicObject t, Object... params) {
                ABSDynamicClass cls = (ABSDynamicClass)t.getFieldValue_Internal("class");
                return cls.getName();
            }
        });

        thisClass.addMethod(/*ABSClosure*/ "getMethod", new ABSClosure() {
            @Override
            public ABSDynamicObject exec(ABSDynamicObject t, Object... params) {
                ABSDynamicClass cls = (ABSDynamicClass)t.getFieldValue_Internal("class");
                String name = (String)params[0];
                ABSClosure method = cls.getMethod(name);
                return method;
            }
        });

        thisClass.addMethod(/*ABSUnit*/ "addMethod", new ABSClosure() {
            @Override
            public ABSUnit exec(ABSDynamicObject t, Object... params) {
                ABSDynamicClass cls = (ABSDynamicClass)t.getFieldValue_Internal("class");
                String name = (String)params[0];
                ABSClosure method = (ABSClosure)((ABSDynamicObject)params[1]).getFieldValue_Internal("method");
                cls.addMethod(name, method);
                return ABSUnit.UNIT;
            }
        });

        thisClass.addMethod(/*ABSUnit*/ "removeMethod", new ABSClosure() {
            @Override
            public ABSUnit exec(ABSDynamicObject t, Object... params) {
                ABSDynamicClass cls = (ABSDynamicClass)t.getFieldValue_Internal("class");
                String name = (String)params[0];
                cls.removeMethod(name);
                return ABSUnit.UNIT;
            }
        });

    }

}
