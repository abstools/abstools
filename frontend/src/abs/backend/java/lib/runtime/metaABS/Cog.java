/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.lib.runtime.metaABS;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.logging.Logger;

import abs.backend.java.codegeneration.dynamic.DynamicException;
import abs.backend.java.lib.runtime.ABSClosure;
import abs.backend.java.lib.runtime.ABSDynamicClass;
import abs.backend.java.lib.runtime.ABSDynamicObject;
import abs.backend.java.lib.runtime.COG;
import abs.backend.java.lib.runtime.Logging;
import abs.backend.java.lib.types.ABSProcess;
import abs.backend.java.lib.types.ABSUnit;
import abs.backend.java.lib.types.ABSValue;
import abs.backend.java.scheduling.SimpleTaskScheduler;
import abs.backend.java.scheduling.TaskScheduler;
import abs.backend.java.scheduling.TaskSchedulingStrategy;
import abs.backend.java.scheduling.SimpleTaskScheduler.TaskInfo;

public class Cog {
    private final static Logger logger = Logging.getLogger(Cog.class.getName());
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
                if (! (cog.getScheduler() instanceof SimpleTaskScheduler)) {
                    throw new DynamicException("For user-defined scheduling to work, the Scheduler must be an instance of " + SimpleTaskScheduler.class.getName() + " (use \"-taskscheduler=simple\")");
                }


                // The user-defined scheduler
                final ABSDynamicObject userScheduler = (ABSDynamicObject)params[0];

                // Create a new scheduling strategy that invokes the user-defined scheduler's "schedule" method
                // - convert TaskSchedulingStrategy's List<TaskInfo> to ProcessScheduler's List<Process>
                // - convert returned Process to TaskInfo.


                // custom TaskSchedulingStrategy
                TaskSchedulingStrategy strategy = new TaskSchedulingStrategy() {
                    @Override
                    public synchronized TaskInfo schedule(final TaskScheduler scheduler, final List<TaskInfo> schedulableTasks) {
                        System.out.println("Scheduling (" + schedulableTasks.size() + " processes in queue)...");

                        // Remember TaskInfos based on their Pids to speed things up a little
                        HashMap<Long, TaskInfo> taskMap = new HashMap<>();

                        // Convert List<TaskInfo> to ArrayList<ABSProcess>
                        ArrayList<ABSProcess> processes = new ArrayList<>();

                        for (TaskInfo task : schedulableTasks) {
                            taskMap.put(task.id, task);

                            // TODO set: pid, method, arrival, cost, deadline, start, finish, critical, value
                            ABSProcess proc = new ABSProcess(
                                    task.task.getID(),
                                    task.task.getCall().methodName(),
                                    0,0,0,0,0,false,0);

                            processes.add(proc);
                            System.out.println("\t" + proc.toString());
                        }

                        /*
                        // Convert ArryList<ABSProcess> to ABS.StdLib.List of ABSProcess
                        final ABS.StdLib.List queue = ListUtils.toABSList(processes);
                        ABSValue result = userScheduler.dispatch("schedule", queue);

                        // Convert returned ABSValue (actually an ABSProcess) to TaskInfo
                        long selectedPid = ((ABSProcess)result).getPid();
                        return taskMap.get(selectedPid);
                        */

                        // FIXME
                        return schedulableTasks.get(0);
                    }
                };

                // Connect the strategy to the cog's scheduler
                System.out.println(cog.toString() + ": Setting TaskSchedulingStrategy to: " + userScheduler.getClassName());

                // move the userScheduler object to the Cog
                // this WILL cause problems if the user associates the scheduler with multiple cogs!!
                //userScheduler.setCOG(cog);

                SimpleTaskScheduler scheduler = (SimpleTaskScheduler)cog.getScheduler();
                scheduler.setSchedulingStrategy(strategy);
                return ABSUnit.UNIT;
            }
        });

    }

}
