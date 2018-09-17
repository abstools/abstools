/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.lib.runtime.metaABS;

import java.util.List;

import org.abs_models.backend.java.lib.runtime.ABSClosure;
import org.abs_models.backend.java.lib.runtime.ABSDynamicClass;
import org.abs_models.backend.java.lib.runtime.ABSDynamicDelta;
import org.abs_models.backend.java.lib.runtime.ABSDynamicObject;
import org.abs_models.backend.java.lib.runtime.ABSDynamicProduct;
import org.abs_models.backend.java.lib.runtime.ABSDynamicReconfiguration;
import org.abs_models.backend.java.lib.runtime.ABSDynamicUpdate;
import org.abs_models.backend.java.lib.types.ABSString;
import org.abs_models.backend.java.lib.types.ABSUnit;
import org.abs_models.backend.java.lib.types.ABSValue;
import org.abs_models.common.ListUtils;

public class Reconfiguration {

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
        thisClass.setName("Reconfiguration");
    
        /* 
         * MetaABS Reconfiguration API -- cf. abslang.abs module ABS.Meta 
         */
        
        thisClass.addMethod(/*ABSString*/ "getName", new ABSClosure() {
            @Override
            public ABSString exec(ABSDynamicObject t, ABSValue... params) {
                ABSDynamicReconfiguration thisR = (ABSDynamicReconfiguration)t;
                return ABSString.fromString(thisR.getName());
            }
        });

        thisClass.addMethod(/*ABSDynamicProduct*/ "getCurrentProduct", new ABSClosure() {
            @Override
            public ABSDynamicProduct exec(ABSDynamicObject t, ABSValue... params) {
                ABSDynamicReconfiguration thisR = (ABSDynamicReconfiguration)t;
                return thisR.getCurrentProduct();
            }
        });

        thisClass.addMethod(/*ABSUnit*/ "setCurrentProduct", new ABSClosure() {
            @Override
            public ABSUnit exec(ABSDynamicObject t, ABSValue... params) {
                ABSDynamicReconfiguration thisR = (ABSDynamicReconfiguration)t;
                ABSDynamicProduct prod = (ABSDynamicProduct)params[0];
                thisR.setCurrentProduct(prod);
                return ABSUnit.UNIT;
            }
        });

        thisClass.addMethod(/*ABSDynamicProduct*/ "getTargetProduct", new ABSClosure() {
            @Override
            public ABSDynamicProduct exec(ABSDynamicObject t, ABSValue... params) {
                ABSDynamicReconfiguration thisR = (ABSDynamicReconfiguration)t;
                return thisR.getTargetProduct();
            }
        });

        thisClass.addMethod(/*ABSUnit*/ "setTargetProduct", new ABSClosure() {
            @Override
            public ABSUnit exec(ABSDynamicObject t, ABSValue... params) {
                ABSDynamicReconfiguration thisR = (ABSDynamicReconfiguration)t;
                ABSDynamicProduct prod = (ABSDynamicProduct)params[0];
                thisR.setTargetProduct(prod);
                return ABSUnit.UNIT;
            }
        });
        
        thisClass.addMethod(/*List<ABSDynamicDelta>*/ "getDeltas", new ABSClosure() {
            @Override
            public ABSValue exec(ABSDynamicObject t, ABSValue... params) {
                ABSDynamicReconfiguration thisR = (ABSDynamicReconfiguration)t;
                List<ABSDynamicDelta> deltas = thisR.getDeltas();
                return ListUtils.toABSList(deltas);
            }
        });

        thisClass.addMethod(/*List<ABSDynamicDelta>*/ "setDeltas", new ABSClosure() {
            @Override
            public ABSValue exec(ABSDynamicObject t, ABSValue... params) {
                ABSDynamicReconfiguration thisR = (ABSDynamicReconfiguration)t;
                // TODO
                return ABSUnit.UNIT;
            }
        });

        thisClass.addMethod(/*ABSDynamicUpdate*/ "getStateUpdate", new ABSClosure() {
            @Override
            public ABSDynamicUpdate exec(ABSDynamicObject t, ABSValue... params) {
                ABSDynamicReconfiguration thisR = (ABSDynamicReconfiguration)t;
                return thisR.getUpdate();
            }
        });
        
        thisClass.addMethod(/*List<ABSDynamicDelta>*/ "setStateUpdate", new ABSClosure() {
            @Override
            public ABSValue exec(ABSDynamicObject t, ABSValue... params) {
                ABSDynamicReconfiguration thisR = (ABSDynamicReconfiguration)t;
                ABSDynamicUpdate upd = (ABSDynamicUpdate)params[0];
                thisR.setUpdate(upd);
                return ABSUnit.UNIT;
            }
        });
    
    }

}
