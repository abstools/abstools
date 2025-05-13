/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.lib.runtime.metaABS;

import org.abs_models.backend.java.codegeneration.dynamic.DynamicException;
import org.abs_models.backend.java.lib.runtime.ABSClosure;
import org.abs_models.backend.java.lib.runtime.ABSDynamicClass;
import org.abs_models.backend.java.lib.runtime.ABSDynamicObject;
import org.abs_models.backend.java.lib.runtime.ABSDynamicProduct;
import org.abs_models.backend.java.lib.runtime.ABSDynamicProductLine;
import org.abs_models.backend.java.lib.runtime.ABSDynamicReconfiguration;
import org.abs_models.backend.java.lib.runtime.ABSDynamicRuntime;
import org.abs_models.backend.java.lib.runtime.ABSRuntime;
import org.abs_models.backend.java.lib.types.ABSUnit;

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

    private static ABSDynamicRuntime getDynamicRuntime() {
        ABSRuntime runtime = ABSRuntime.getRuntime();
        if (runtime instanceof ABSDynamicRuntime dr) return dr;
        else throw new DynamicException("Dynamic ABS Java code detected. Please run with -dynamic switch.");
    }

    private static void setupAPI() {
        thisClass.setName("ProductLine");
        
        /* 
         * MetaABS ProductLine API -- cf. abslang.abs module ABS.Meta 
         */
        
        thisClass.addMethod(/*ABSDynamicProduct*/ "getCurrentProduct", new ABSClosure() {
            @Override
            public ABSDynamicProduct exec(ABSDynamicObject t, Object... params) {
                ABSDynamicProduct currentProd = getDynamicRuntime().getDSPL().getCurrentProduct();
                return currentProd;
            }
        });

        thisClass.addMethod(/*ABSDynamicProduct*/ "getProduct", new ABSClosure() {
            @Override
            public ABSDynamicProduct exec(ABSDynamicObject t, Object... params) {
                String name = (String)params[0];
                ABSDynamicProduct product = getDynamicRuntime().getDSPL().getProduct(name);
                return product;
            }
        });
        
        thisClass.addMethod(/*ABSUnit*/ "reconfigure", new ABSClosure() {
            @Override
            public ABSUnit exec(ABSDynamicObject t, Object... params) {
                ABSDynamicProduct targetProd = (ABSDynamicProduct)params[0];
                ABSDynamicProductLine pl = getDynamicRuntime().getDSPL();
                pl.reconfigure(targetProd);
                return ABSUnit.UNIT;
            }
        });
        
        thisClass.addMethod(/*ABSUnit*/ "addProduct", new ABSClosure() {
            @Override
            public ABSUnit exec(ABSDynamicObject t, Object... params) {
                ABSDynamicProduct product = (ABSDynamicProduct)params[0];
                getDynamicRuntime().getDSPL().addProduct(product);
                return ABSUnit.UNIT;
            }
        });
        
        thisClass.addMethod(/*ABSUnit*/ "removeProduct", new ABSClosure() {
            @Override
            public ABSUnit exec(ABSDynamicObject t, Object... params) {
                ABSDynamicProduct product = (ABSDynamicProduct)params[0];
                getDynamicRuntime().getDSPL().removeProduct(product);
                return ABSUnit.UNIT;
            }
        });
        
        thisClass.addMethod(/*ABSUnit*/ "addReconfiguration", new ABSClosure() {
            @Override
            public ABSUnit exec(ABSDynamicObject t, Object... params) {
                ABSDynamicReconfiguration recf = (ABSDynamicReconfiguration)params[0];
                getDynamicRuntime().getDSPL().addReconfiguration(recf);
                return ABSUnit.UNIT;
            }
        });
        
        thisClass.addMethod(/*ABSUnit*/ "removeReconfiguration", new ABSClosure() {
            @Override
            public ABSUnit exec(ABSDynamicObject t, Object... params) {
                ABSDynamicReconfiguration recf = (ABSDynamicReconfiguration)params[0];
                getDynamicRuntime().getDSPL().removeReconfiguration(recf);
                return ABSUnit.UNIT;
            }
        });
    }
    
}
