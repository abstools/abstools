/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.lib.runtime.metaABS;

import abs.backend.java.lib.runtime.ABSClosure;
import abs.backend.java.lib.runtime.ABSDynamicClass;
import abs.backend.java.lib.runtime.ABSDynamicObject;
import abs.backend.java.lib.runtime.COG;
import abs.backend.java.lib.types.ABSUnit;
import abs.backend.java.lib.types.ABSValue;
import abs.backend.java.scheduling.SimpleTaskScheduler;
import abs.backend.java.scheduling.TaskScheduler;
import abs.backend.java.scheduling.TaskSchedulingStrategy;

public class Cog {
    private static ABSDynamicClass thisClass;

    /* 
     * Create the class object. This only happens once.
     */
    public static ABSDynamicClass singleton() {
        if (thisClass == null) {
            thisClass = new ABSDynamicClass();
            setupAPI();
        }
        return thisClass;
    }

    private static void setupAPI() {
        thisClass.setName("Cog");
        
        thisClass.addMethod(/*List<Process>*/ "getQueue", new ABSClosure() {
            @Override
            public ABSValue exec(ABSDynamicObject t, ABSValue... params) {
                
                // TODO
                return ABSUnit.UNIT;
            }
        });
        
        thisClass.addMethod(/*ABSUnit*/ "info", new ABSClosure() {
            @Override
            public ABSValue exec(ABSDynamicObject t, ABSValue... params) {
                COG cog = (COG)t.getFieldValue_Internal("cog");
                
                System.out.println("Cog scheduler " + cog.getScheduler().toString());
                
                if (cog.getScheduler() instanceof SimpleTaskScheduler) {
                    SimpleTaskScheduler sch = (SimpleTaskScheduler)cog.getScheduler();
                    TaskSchedulingStrategy strat = sch.getSchedulingStrategy();
                    System.out.println("Strategy " + strat);
                }
                
                return ABSUnit.UNIT;
            }
        });
    }
    
}
