/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.lib.runtime;

import abs.backend.java.lib.types.*;

/* Class ABSObjectMirror
 * 
 * The builtin ABS function reflect(o) creates an object mirror for
 * the given object o. Here we (dynamically) define the class of the meta-object,
 * including its interface, i.e. the meta-object protocol for object mirrors.
 */
public class ABSObjectMirrorClass {
    private static ABSDynamicClass objectMirrorClass;

    /* 
     * Create the class object for object mirrors. This only happens once.
     */
    public static ABSDynamicClass singleton() {
        if (objectMirrorClass == null) {
            // System.err.println("Creating ObjectMirrorClass and metaAPI.");
            objectMirrorClass = new ABSDynamicClass();
            setupMetaAPI();
        }
        return objectMirrorClass;
    }
    
    public static void setupMetaAPI() {
        objectMirrorClass.setName("ObjectMirror");
        
        /*
         * getClassName: get name of object's class
         */
        objectMirrorClass.addMethod("getClassName", new ABSClosure() {
            @Override
            public ABSString exec(ABSDynamicObject mirror, ABSValue... params) {
                ABSString name;
                name = ABSString.fromString(((ABSDynamicObject)mirror.dispatch("getObject")).getClassName());
                return name;
            }
        });
        
        /*
         * getClass: get class of object
         */
        objectMirrorClass.addMethod("getClass", new ABSClosure() {
            @Override
            public ABSClass exec(ABSDynamicObject mirror, ABSValue... params) {
                return (ABSClass)((ABSDynamicObject)mirror.dispatch("getObject")).getClazz();
            }
        });
        
        /*
         * setClass: set new class for object
         */
        objectMirrorClass.addMethod("setClass", new ABSClosure() {
            @Override
            public ABSUnit exec(ABSDynamicObject mirror, ABSValue... params) {
                ((ABSDynamicObject)mirror.dispatch("getObject")).setClazz((ABSDynamicClass)params[0]);
                return ABSUnit.UNIT;
            }
        });

        // TODO: getFieldValue(), setFieldValue()

        objectMirrorClass.addMethod("getCog", new ABSClosure() {
            @Override
            public ABSValue exec(ABSDynamicObject mirror, ABSValue... params) {
                return ((ABSDynamicObject)mirror.dispatch("getObject")).getCOG();
            }
        });
        
        objectMirrorClass.addMethod("setCog", new ABSClosure() {
            @Override
            public ABSUnit exec(ABSDynamicObject mirror, ABSValue... params) {
                ((ABSDynamicObject)mirror.dispatch("getObject")).setCOG((COG)params[0]);
                return ABSUnit.UNIT;
            }
        });

        /*
         * getObject: obtain the mirrored object
         */
        objectMirrorClass.addMethod("getObject", new ABSClosure() {
            @Override
            public ABSDynamicObject exec(ABSDynamicObject mirror, ABSValue... params) {
                ABSDynamicObject object;
                try {
                    object = (ABSDynamicObject)mirror.getFieldValue("object");
                } catch (NoSuchFieldException e) {
                    object = null; // should not happen
                }
                return object;
            }
        });

        /*
         * print debug statement to STDERR
         */
        objectMirrorClass.addMethod("debug", new ABSClosure() {
            @Override
            public ABSValue exec(ABSDynamicObject t, ABSValue... params) {
                System.err.println(params[0]);
                return ABSUnit.UNIT;
            }
        });
        
    }


}
