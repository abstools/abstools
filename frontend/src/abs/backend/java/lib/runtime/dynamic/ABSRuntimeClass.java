/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.lib.runtime.dynamic;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import abs.backend.java.codegeneration.dynamic.DynamicException;
import abs.backend.java.lib.runtime.ABSClosure;
import abs.backend.java.lib.runtime.ABSDynamicClass;
import abs.backend.java.lib.runtime.ABSDynamicObject;
import abs.backend.java.lib.types.*;

public class ABSRuntimeClass {
    private static ABSDynamicClass runtimeClass;

    /* 
     * Create the class object for object mirrors. This only happens once.
     */
    public static ABSDynamicClass singleton() {
        if (runtimeClass == null) {
            runtimeClass = new ABSDynamicClass();
            setupAPI();
        }
        return runtimeClass;
    }

    public static void setupAPI() {
        runtimeClass.setName("Runtime");
        
        runtimeClass.addMethod("applyDelta", new ABSClosure() {
            @Override
            public ABSValue exec(ABSDynamicObject t, ABSValue... params) {
                String deltaName = ((ABSString)params[0]).getString();
                System.err.println("*** Applying delta." + deltaName);
                String className = "delta." + deltaName + ".Application";
                
                try {
                    Class<?> clazz = Class.forName(className);
                    Method method;
                    try {
                        method = clazz.getDeclaredMethod("apply", new Class[0]);
                        try {
                            method.invoke(null, new Object[0]);
                        } catch (IllegalArgumentException e) {
                            // TODO Auto-generated catch block
                            e.printStackTrace();
                        } catch (IllegalAccessException e) {
                            // TODO Auto-generated catch block
                            e.printStackTrace();
                        } catch (InvocationTargetException e) {
                            // TODO Auto-generated catch block
                            e.printStackTrace();
                        }
                    } catch (SecurityException e1) {
                        // TODO Auto-generated catch block
                        e1.printStackTrace();
                    } catch (NoSuchMethodException e1) {
                        // TODO Auto-generated catch block
                        e1.printStackTrace();
                    }
                    
                } catch (ClassNotFoundException e) {
                    throw new DynamicException("Error loading class: " + className);
                }
                
                return ABSUnit.UNIT;
            }
        });
        
    }
}
