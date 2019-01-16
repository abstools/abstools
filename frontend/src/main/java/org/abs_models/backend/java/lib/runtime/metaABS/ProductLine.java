/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.lib.runtime.metaABS;

import org.abs_models.backend.java.lib.runtime.ABSClosure;
import org.abs_models.backend.java.lib.runtime.ABSDynamicClass;
import org.abs_models.backend.java.lib.runtime.ABSDynamicObject;
import org.abs_models.backend.java.lib.runtime.ABSDynamicProduct;
import org.abs_models.backend.java.lib.runtime.ABSDynamicProductLine;
import org.abs_models.backend.java.lib.runtime.ABSDynamicReconfiguration;
import org.abs_models.backend.java.lib.types.ABSString;
import org.abs_models.backend.java.lib.types.ABSUnit;
import org.abs_models.backend.java.lib.types.ABSValue;

public class ProductLine {
    private static ABSDynamicClass thisClass;

    /* 
     * Create the singleton "ProductLine" class object
     */
    public static ABSDynamicClass singleton() {
        if (thisClass == null) {
            thisClass = new ABSDynamicClass();
            setupAPI();
        }
        return thisClass;
    }

    private static void setupAPI() {
        thisClass.setName("ProductLine");
        
        /* 
         * MetaABS ProductLine API -- cf. abslang.abs module ABS.Meta 
         */
        
        thisClass.addMethod(/*ABSDynamicProduct*/ "getCurrentProduct", new ABSClosure() {
            @Override
            public ABSDynamicProduct exec(ABSDynamicObject t, ABSValue... params) {
                ABSDynamicProduct currentProd = t.__ABS_getRuntime().getDSPL().getCurrentProduct();
                return currentProd;
            }
        });

        thisClass.addMethod(/*ABSDynamicProduct*/ "getProduct", new ABSClosure() {
            @Override
            public ABSDynamicProduct exec(ABSDynamicObject t, ABSValue... params) {
                ABSString name = (ABSString)params[0];
                ABSDynamicProduct product = t.__ABS_getRuntime().getDSPL().getProduct(name.getString());
                return product;
            }
        });
        
        thisClass.addMethod(/*ABSUnit*/ "reconfigure", new ABSClosure() {
            @Override
            public ABSUnit exec(ABSDynamicObject t, ABSValue... params) {
                ABSDynamicProduct targetProd = (ABSDynamicProduct)params[0];
                ABSDynamicProductLine pl = t.__ABS_getRuntime().getDSPL();
                pl.reconfigure(targetProd);
                return ABSUnit.UNIT;
            }
        });
        
        thisClass.addMethod(/*ABSUnit*/ "addProduct", new ABSClosure() {
            @Override
            public ABSUnit exec(ABSDynamicObject t, ABSValue... params) {
                ABSDynamicProduct product = (ABSDynamicProduct)params[0];
                t.__ABS_getRuntime().getDSPL().addProduct(product);
                return ABSUnit.UNIT;
            }
        });
        
        thisClass.addMethod(/*ABSUnit*/ "removeProduct", new ABSClosure() {
            @Override
            public ABSUnit exec(ABSDynamicObject t, ABSValue... params) {
                ABSDynamicProduct product = (ABSDynamicProduct)params[0];
                t.__ABS_getRuntime().getDSPL().removeProduct(product);
                return ABSUnit.UNIT;
            }
        });
        
        thisClass.addMethod(/*ABSUnit*/ "addReconfiguration", new ABSClosure() {
            @Override
            public ABSUnit exec(ABSDynamicObject t, ABSValue... params) {
                ABSDynamicReconfiguration recf = (ABSDynamicReconfiguration)params[0];
                t.__ABS_getRuntime().getDSPL().addReconfiguration(recf);
                return ABSUnit.UNIT;
            }
        });
        
        thisClass.addMethod(/*ABSUnit*/ "removeReconfiguration", new ABSClosure() {
            @Override
            public ABSUnit exec(ABSDynamicObject t, ABSValue... params) {
                ABSDynamicReconfiguration recf = (ABSDynamicReconfiguration)params[0];
                t.__ABS_getRuntime().getDSPL().removeReconfiguration(recf);
                return ABSUnit.UNIT;
            }
        });
    }
    
}
