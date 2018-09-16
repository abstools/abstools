/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.scheduling;

import java.util.List;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.logging.Logger;

import org.abs_models.backend.java.lib.runtime.Logging;
import org.abs_models.backend.java.lib.types.ABSProcess;
import org.abs_models.common.ListUtils;


public abstract class UserSchedulingStrategy implements TaskSchedulingStrategy {
    private final static Logger logger = Logging.getLogger(UserSchedulingStrategy.class.getName());

    @Override
    public synchronized SimpleTaskScheduler.TaskInfo schedule(final TaskScheduler scheduler, final List<SimpleTaskScheduler.TaskInfo> schedulableTasks) {
        System.out.println(scheduler.getCOG().toString() + " Scheduling (" + schedulableTasks.size() + " processes in queue)...");

        // Remember TaskInfos based on their Pids to speed things up a little
        HashMap<Integer, SimpleTaskScheduler.TaskInfo> taskMap = new HashMap<>();

        // Convert List<TaskInfo> to ArrayList<ABSProcess>
        ArrayList<ABSProcess> processes = new ArrayList<>();

        for (SimpleTaskScheduler.TaskInfo taskInfo : schedulableTasks) {
            taskMap.put(taskInfo.task.getID(), taskInfo);

            // TODO set: pid, method, arrival, cost, deadline, start, finish, critical, value
            ABSProcess proc = new ABSProcess(
                    taskInfo.task.getID(), //Or use taskInfo.id (long)??
                    taskInfo.task.getCall().methodName(),
                    taskInfo.task.getArrival(),
                    taskInfo.task.getCost(),
                    taskInfo.task.getDeadline(),
                    taskInfo.task.getStart(),
                    taskInfo.task.getFinish(),
                    taskInfo.task.isCritical(),
                    taskInfo.task.getValue());

            processes.add(proc);
        }
        logger.info(processes.toString());

        // Convert ArryList<ABSProcess> to ABS.StdLib.List of ABSProcess
        final Object queue = ListUtils.toABSList(processes);
        ABSProcess result = userschedule(queue);

        // Convert returned ABSValue (actually an ABSProcess) to TaskInfo
        int selectedPid = result.getPid();
        logger.info("scheduling Task " + selectedPid);
        return taskMap.get(selectedPid);
    }

    // to be implemented by the generated subclass
    protected abstract ABSProcess userschedule(final Object queue);


}

