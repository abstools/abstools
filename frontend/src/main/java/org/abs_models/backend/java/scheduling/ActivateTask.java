/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.scheduling;

import org.abs_models.backend.java.lib.runtime.COG;
import org.abs_models.backend.java.lib.runtime.Task;
import org.abs_models.backend.java.observing.TaskView;

public class ActivateTask extends ScheduleAction {

    private final Task<?> task;

    public ActivateTask(COG cog, Task<?> task) {
        super(cog);
        if (task == null) {
            throw new IllegalArgumentException("Task is null");
        }
        this.task = task;
         
    }
    
    @Override
    public String toString() {
        return "Activate task "+task.getID();
    }    

    @Override
    public String shortString() {
        return getCOG().getID() + ",A," + task.getID();
    }
    
    @Override
    public TaskView getTask() {
        if (task == null)
            return null;
        return task.getView();
    }

}
