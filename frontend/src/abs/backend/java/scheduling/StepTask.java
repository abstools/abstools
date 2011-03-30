/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.scheduling;

import abs.backend.java.lib.runtime.Task;
import abs.backend.java.observing.TaskView;

public class StepTask extends ScheduleAction {
    private final Task<?> task;

    public StepTask(Task<?> task) {
        super(task.getCOG());
        this.task = task;
    }

    public TaskView getTask() {
        if (task == null)
            return null;
        return task.getView();
    }

    public String toString() {
        return "Step Task " + task.getID();
    }

    @Override
    public String shortString() {
        return getCOG().getID() + ",E," + task.getID();
    }

    public boolean equals(Object o) {
        if (!(o instanceof StepTask))
            return false;
        StepTask t = (StepTask) o;
        return t.task.equals(this.task);
    }

}
