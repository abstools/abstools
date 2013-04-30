/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.lib.runtime.metaABS;

import abs.backend.java.lib.runtime.ABSClosure;
import abs.backend.java.lib.runtime.ABSDynamicClass;
import abs.backend.java.lib.runtime.ABSDynamicFeature;
import abs.backend.java.lib.runtime.ABSDynamicObject;
import abs.backend.java.lib.types.ABSString;
import abs.backend.java.lib.types.ABSValue;

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
            public ABSString exec(ABSDynamicObject t, ABSValue... params) {
                ABSDynamicFeature f = (ABSDynamicFeature)t;
                return ABSString.fromString(f.getName());
            }
        });

        thisClass.addMethod(/*ABSString*/ "getAttributes", new ABSClosure() {
            @Override
            public ABSString exec(ABSDynamicObject t, ABSValue... params) {
                ABSDynamicFeature f = (ABSDynamicFeature)t;

                //TODO
                return ABSString.fromString("Not Implemented Yet");
            }
        });

    }

}
