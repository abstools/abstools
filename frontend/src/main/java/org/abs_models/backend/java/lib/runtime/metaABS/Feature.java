/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.lib.runtime.metaABS;

import org.abs_models.backend.java.lib.runtime.ABSClosure;
import org.abs_models.backend.java.lib.runtime.ABSDynamicClass;
import org.abs_models.backend.java.lib.runtime.ABSDynamicFeature;
import org.abs_models.backend.java.lib.runtime.ABSDynamicObject;

public class Feature {

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
        thisClass.setName("Feature");

        thisClass.addMethod(/*ABSString*/ "getName", new ABSClosure() {
            @Override
            public String exec(ABSDynamicObject t, Object... params) {
                ABSDynamicFeature f = (ABSDynamicFeature)t;
                return f.getName();
            }
        });

        thisClass.addMethod(/*ABSString*/ "getAttributes", new ABSClosure() {
            @Override
            public String exec(ABSDynamicObject t, Object... params) {
                ABSDynamicFeature f = (ABSDynamicFeature)t;

                //TODO
                return "Not Implemented Yet";
            }
        });

    }

}
