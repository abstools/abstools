/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.lib.runtime;

import abs.backend.java.lib.types.*;

/* The builtin ABS function reflect(o) creates a meta-object (a.k.a. object mirror) for
 * the given object o. Here we (dynamically) define the class of the meta-object,
 * including its interface, i.e. the meta-object protocol for object mirrors.
 */
public class ABSMetaObject {
    private static ABSClass objectMirrorClass;

    public static ABSClass getObjectMirrorClass() {
        if (objectMirrorClass == null) {
            objectMirrorClass = new ABSClass();
            setupMetaAPI();
        }
        return objectMirrorClass;
    }
    
    public static void setupMetaAPI() {
        objectMirrorClass.setName("ObjectMirror");
        
        objectMirrorClass.addMethod("getClassName", new ABSClosure() {
            @Override
            public ABSString exec(ABSDynamicObject t, ABSValue... params) {
                return ABSString.fromString(t.getClassName());
            }
        });
            
        objectMirrorClass.addMethod("getClass", new ABSClosure() {
            @Override
            public ABSValue exec(ABSDynamicObject t, ABSValue... params) {
                return (ABSValue)t.getClazz();
            }
        });
        
        objectMirrorClass.addMethod("setClass", new ABSClosure() {
            @Override
            public ABSUnit exec(ABSDynamicObject t, ABSValue... params) {
                t.setClazz((ABSClass)params[0]);
                return ABSUnit.UNIT;
            }
        });

        // TODO: getFieldValue(), setFieldValue()
    }




}
