/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.lib.runtime.metaABS;

import abs.backend.java.lib.runtime.ABSClosure;
import abs.backend.java.lib.runtime.ABSDynamicClass;
import abs.backend.java.lib.runtime.ABSDynamicObject;
import abs.backend.java.lib.types.ABSString;
import abs.backend.java.lib.types.ABSUnit;
import abs.backend.java.lib.types.ABSValue;

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

        thisClass.addMethod(/*ABSString*/ "getName", new ABSClosure() {
            @Override
            public ABSString exec(ABSDynamicObject t, ABSValue... params) {
                ABSDynamicClass cls = (ABSDynamicClass)t.getFieldValue_Internal("class");
                return ABSString.fromString(cls.getName());
            }
        });

        thisClass.addMethod(/*ABSClosure*/ "getMethod", new ABSClosure() {
            @Override
            public ABSDynamicObject exec(ABSDynamicObject t, ABSValue... params) {
                ABSDynamicClass cls = (ABSDynamicClass)t.getFieldValue_Internal("class");
                ABSString name = (ABSString)params[0];
                ABSClosure method = cls.getMethod(name.getString());
                return method;
            }
        });

        thisClass.addMethod(/*ABSUnit*/ "addMethod", new ABSClosure() {
            @Override
            public ABSUnit exec(ABSDynamicObject t, ABSValue... params) {
                ABSDynamicClass cls = (ABSDynamicClass)t.getFieldValue_Internal("class");
                ABSString name = (ABSString)params[0];
                ABSClosure method = (ABSClosure)((ABSDynamicObject)params[1]).getFieldValue_Internal("method");
                cls.addMethod(name.getString(), method);
                return ABSUnit.UNIT;
            }
        });

        thisClass.addMethod(/*ABSUnit*/ "removeMethod", new ABSClosure() {
            @Override
            public ABSUnit exec(ABSDynamicObject t, ABSValue... params) {
                ABSDynamicClass cls = (ABSDynamicClass)t.getFieldValue_Internal("class");
                ABSString name = (ABSString)params[0];
                cls.removeMethod(name.getString());
                return ABSUnit.UNIT;
            }
        });

    }

}
