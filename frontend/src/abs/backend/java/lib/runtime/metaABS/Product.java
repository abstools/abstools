/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.lib.runtime.metaABS;

import java.util.ArrayList;
import java.util.List;

import abs.backend.java.lib.runtime.ABSClosure;
import abs.backend.java.lib.runtime.ABSDynamicClass;
import abs.backend.java.lib.runtime.ABSDynamicDelta;
import abs.backend.java.lib.runtime.ABSDynamicFeature;
import abs.backend.java.lib.runtime.ABSDynamicObject;
import abs.backend.java.lib.runtime.ABSDynamicProduct;
import abs.backend.java.lib.runtime.ABSDynamicReconfiguration;
import abs.backend.java.lib.types.ABSString;
import abs.backend.java.lib.types.ABSValue;
import abs.common.ListUtils;

public class Product {
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
        thisClass.setName("Product");

        /*
         * MetaABS Product API -- cf. abslang.abs module ABS.Meta
         */

        thisClass.addMethod(/*ABSString*/ "getName", new ABSClosure() {
            @Override
            public ABSString exec(ABSDynamicObject t, ABSValue... params) {
                ABSDynamicProduct thisP = (ABSDynamicProduct)t;
                return ABSString.fromString(thisP.getName());
            }
        });

        thisClass.addMethod(/*List<ABSDynamicFeature>*/ "getFeatures", new ABSClosure() {
            @Override
            public ABSValue exec(ABSDynamicObject t, ABSValue... params) {
                ABSDynamicProduct thisP = (ABSDynamicProduct)t;
                ArrayList<ABSDynamicFeature> features = new ArrayList<>();
                for (ABSDynamicFeature f : thisP.getFeatures()) {
                    features.add(f);
                }
                return ListUtils.toABSList(features);
            }
        });

        thisClass.addMethod(/*Set<ABSDynamicProduct>*/ "getConfigurableProducts", new ABSClosure() {
            @Override
            public ABSValue exec(ABSDynamicObject t, ABSValue... params) {
                ABSDynamicProduct thisP = (ABSDynamicProduct)t;
                return ListUtils.toABSSet(thisP.getConfigurableProducts());
            }
        });

        thisClass.addMethod(/*ABSDynamicReconfiguration*/ "getReconfiguration", new ABSClosure() {
            @Override
            public ABSDynamicReconfiguration exec(ABSDynamicObject t, ABSValue... params) {
                ABSDynamicProduct thisP = (ABSDynamicProduct)t;
                ABSDynamicProduct targetP = (ABSDynamicProduct)params[0];
                return thisP.getReconfiguration(targetP);
            }
        });

    }

}
