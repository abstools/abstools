/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.scheduling;

import java.util.List;

import abs.backend.java.observing.TaskSchedulerView;
import abs.backend.java.scheduling.SimpleTaskScheduler.TaskInfo;

public class RecordingSchedulerStrategy implements TaskSchedulingStrategy {

    private final TaskSchedulingStrategy schedulingStrat;

    public RecordingSchedulerStrategy(TaskSchedulingStrategy s) {
        this.schedulingStrat = s;
    }

    @Override
    public TaskInfo schedule(TaskScheduler scheduler, List<TaskInfo> scheduableTasks) {
        int cogId = scheduler.getCOG().getID();

        TaskInfo choosenTask = schedulingStrat.schedule(scheduler, scheduableTasks);
        long taskId = choosenTask.id;
        String suff = scheduableTasks.size() == 1 ? " (NO CHOICE)" : "";

        System.out.println("COG " + cogId + " (" + scheduler.getCOG().getInitialClass().getName()
                + "): Scheduled task " + taskId + " from {" + tasksToStringList(cogId, scheduableTasks) + "}" + suff);
        return choosenTask;
    }

    private String tasksToStringList(int cogid, List<TaskInfo> scheduableTasks) {
        StringBuilder res = new StringBuilder();
        boolean first = true;
        for (TaskInfo info : scheduableTasks) {
            if (first)
                first = false;
            else
                res.append(", ");
            res.append(info.id);
        }

        return res.toString();
    }

}
