/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.lib.runtime.metaABS;

import abs.backend.java.lib.runtime.ABSClosure;
import abs.backend.java.lib.runtime.ABSDynamicClass;
import abs.backend.java.lib.runtime.ABSDynamicObject;
import abs.backend.java.lib.runtime.COG;
import abs.backend.java.lib.types.*;

/* Class ABSObjectMirror
 * 
 * The builtin ABS function reflect(o) creates an object mirror for
 * the given object o. Here we (dynamically) define the class of the meta-object,
 * including its interface, i.e. the meta-object protocol for object mirrors.
 */
public class ObjectMirrorClass {
    private static ABSDynamicClass thisClass;

    /* 
     * Create the class object for object mirrors. This only happens once.
     */
    public static ABSDynamicClass singleton() {
        if (thisClass == null) {
            // System.err.println("Creating ObjectMirrorClass and metaAPI.");
            thisClass = new ABSDynamicClass();
            setupMetaAPI();
        }
        return thisClass;
    }
    
    public static void setupMetaAPI() {
        thisClass.setName("ObjectMirror");
        
        /*
         * getClassName: get name of object's class
         */
        thisClass.addMethod(/*ABSString*/ "getClassName", new ABSClosure() {
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
        thisClass.addMethod(/*ABSClass*/ "getClass", new ABSClosure() {
            @Override
            public ABSClass exec(ABSDynamicObject mirror, ABSValue... params) {
                return (ABSDynamicClass)((ABSDynamicObject)mirror.dispatch("getObject")).getClazz();
            }
        });
        
        /*
         * setClass: set new class for object
         */
        thisClass.addMethod(/*Unit*/ "setClass", new ABSClosure() {
            @Override
            public ABSUnit exec(ABSDynamicObject mirror, ABSValue... params) {
                ((ABSDynamicObject)mirror.dispatch("getObject")).setClazz((ABSDynamicClass)params[0]);
                return ABSUnit.UNIT;
            }
        });

        /*
         * respondsTo: find out whether object responds to given method
         */
        thisClass.addMethod(/*ABSBool*/ "respondsTo", new ABSClosure() {
            @Override
            public ABSBool exec(ABSDynamicObject mirror, ABSValue... params) {
                ABSDynamicClass cls = (ABSDynamicClass)((ABSDynamicObject)mirror.dispatch("getObject")).getClazz();
                return ABSBool.fromBoolean(cls.hasMethod(((ABSString)params[0]).toString()));
            }
        });
        
                
        // TODO: getFieldValue(), setFieldValue()

        thisClass.addMethod(/*COG*/ "getCog", new ABSClosure() {
            @Override
            public ABSValue exec(ABSDynamicObject mirror, ABSValue... params) {
                return ((ABSDynamicObject)mirror.dispatch("getObject")).getCOG();
            }
        });
        
        thisClass.addMethod(/*Unit*/ "setCog", new ABSClosure() {
            @Override
            public ABSUnit exec(ABSDynamicObject mirror, ABSValue... params) {
                ((ABSDynamicObject)mirror.dispatch("getObject")).setCOG((COG)params[0]);
                return ABSUnit.UNIT;
            }
        });

        /*
         * getObject: obtain the mirrored object
         */
        thisClass.addMethod(/*ABSDynamicObject*/ "getObject", new ABSClosure() {
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
    }


}
