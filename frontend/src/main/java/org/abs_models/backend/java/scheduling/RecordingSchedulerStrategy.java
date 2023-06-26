/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.scheduling;

import java.util.List;

public class RecordingSchedulerStrategy implements TaskSchedulingStrategy {

    private final TaskSchedulingStrategy schedulingStrat;

    public RecordingSchedulerStrategy(TaskSchedulingStrategy s) {
        this.schedulingStrat = s;
    }

    @Override
    public SimpleTaskScheduler.TaskInfo schedule(TaskScheduler scheduler, List<SimpleTaskScheduler.TaskInfo> schedulableTasks) {
        int cogId = scheduler.getCOG().getID();

        SimpleTaskScheduler.TaskInfo choosenTask = schedulingStrat.schedule(scheduler, schedulableTasks);
        long taskId = choosenTask.id;
        String suff = schedulableTasks.size() == 1 ? " (NO CHOICE)" : "";

        System.out.println("COG " + cogId + " (" + scheduler.getCOG().getInitialClass().getName()
                + "): Scheduled task " + taskId + " from {" + tasksToStringList(cogId, schedulableTasks) + "}" + suff);
        return choosenTask;
    }

    private String tasksToStringList(int cogid, List<SimpleTaskScheduler.TaskInfo> schedulableTasks) {
        StringBuilder res = new StringBuilder();
        boolean first = true;
        for (SimpleTaskScheduler.TaskInfo info : schedulableTasks) {
            if (first)
                first = false;
            else
                res.append(", ");
            res.append(info.id);
        }

        return res.toString();
    }

}
