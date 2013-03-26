/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.scheduling;

import java.util.List;
import java.util.ArrayList;
import java.util.HashMap;

import abs.backend.java.lib.types.ABSProcess;
import abs.backend.java.scheduling.SimpleTaskScheduler.TaskInfo;
import abs.common.ListUtils;


public abstract class UserSchedulingStrategy implements TaskSchedulingStrategy {

    @Override
    public synchronized TaskInfo schedule(final TaskScheduler scheduler, final List<TaskInfo> schedulableTasks) {
        System.out.println(scheduler.getCOG().toString() + " Scheduling (" + schedulableTasks.size() + " processes in queue)...");

        // Remember TaskInfos based on their Pids to speed things up a little
        HashMap<Long, TaskInfo> taskMap = new HashMap<Long, TaskInfo>();
        
        // Convert List<TaskInfo> to ArrayList<ABSProcess>
        ArrayList<ABSProcess> processes = new ArrayList<ABSProcess>();

        for (TaskInfo taskInfo : schedulableTasks) {
            taskMap.put(taskInfo.id, taskInfo);

            // TODO set: pid, method, arrival, cost, deadline, start, finish, critical, value
            ABSProcess proc = new ABSProcess(
                    taskInfo.id, // Or use task's id -- what's the difference?
                    taskInfo.task.getCall().methodName(),
                    taskInfo.task.getArrival(),
                    taskInfo.task.getCost(),
                    taskInfo.task.getDeadline(),
                    taskInfo.task.getStart(),
                    taskInfo.task.getFinish(),
                    taskInfo.task.isCritical(),
                    taskInfo.task.getValue());

            processes.add(proc);
            System.out.println("\t" + proc.toString());
        }

        // Convert ArryList<ABSProcess> to ABS.StdLib.List of ABSProcess
        final Object queue = ListUtils.toABSList(processes);
        ABSProcess result = userschedule(queue);
        
        // Convert returned ABSValue (actually an ABSProcess) to TaskInfo
        long selectedPid = ((ABSProcess)result).getPid();
        return taskMap.get(selectedPid);
    }
    
    // to be implemented by the generated subclass
    protected abstract ABSProcess userschedule(final Object queue);
    

}

