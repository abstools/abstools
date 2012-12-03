/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.lib.runtime.metaABS;

import java.util.List;

import abs.backend.java.lib.runtime.ABSClosure;
import abs.backend.java.lib.runtime.ABSDynamicClass;
import abs.backend.java.lib.runtime.ABSDynamicObject;
import abs.backend.java.lib.runtime.COG;
import abs.backend.java.lib.types.ABSUnit;
import abs.backend.java.lib.types.ABSValue;
import abs.backend.java.scheduling.SimpleTaskScheduler;
import abs.backend.java.scheduling.TaskScheduler;
import abs.backend.java.scheduling.TaskSchedulingStrategy;
import abs.backend.java.scheduling.SimpleTaskScheduler.TaskInfo;

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
        
        thisClass.addMethod(/*ABSUnit*/ "setScheduler", new ABSClosure() {
            @Override
            public ABSValue exec(ABSDynamicObject t, ABSValue... params) {
                COG cog = (COG)t.getFieldValue_Internal("cog");
                
                // The user-defined scheduler
                final ABSDynamicObject absScheduler = (ABSDynamicObject)params[0];
                System.out.println("ABS user-def Scheduler: class: " + absScheduler.getClassName());
                
                // Create a new scheduling strategy that invokes the user-defined scheduler's scheduler method 
                TaskSchedulingStrategy strategy = new TaskSchedulingStrategy() {
                    @Override
                    public TaskInfo schedule(TaskScheduler scheduler, List<TaskInfo> schedulableTasks) {
                        // TODO
                        System.out.println("ABS Scheduler");
                        
                        System.out.println("scheduling...");
                        System.out.print("\ttasks (" + schedulableTasks.size());
                        System.out.println("): " + schedulableTasks.toString());
                        
                        
                        for (TaskInfo task : schedulableTasks) {
//                            Process p = new Process_Process();
                            
                        }
                        /* 
                         * 
                         * 
                         * 
                         */
                        // The schedule method takes a List<Process>
                        // Process is defined in abslang.abs and generated
                        
                        //return absScheduler.dispatch("schedule", );
                        return schedulableTasks.get(0);
                    }
                };
                
                // Connect the strategy to the cog's scheduler
                if (cog.getScheduler() instanceof SimpleTaskScheduler) {
                    System.out.println("Setting new scheduler");
                    SimpleTaskScheduler scheduler = (SimpleTaskScheduler)cog.getScheduler();
                    scheduler.setSchedulingStrategy(strategy);
                } else {
                    // TODO make sure this case does not happen
                }
                return ABSUnit.UNIT;
            }
        });
    }
    
}
