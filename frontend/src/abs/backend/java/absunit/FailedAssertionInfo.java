/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.absunit;

import abs.backend.java.observing.TaskView;

public class FailedAssertionInfo {
    
    private TaskView failedTask;
    private ScheduleRecord schedule2Failure;
    
    public FailedAssertionInfo(TaskView failedTask, ScheduleRecord schedule2Failure) {
        this.failedTask = failedTask;
        this.schedule2Failure = schedule2Failure;
    }
    
    
    public TaskView getTask() {
        return failedTask;
    }
    
    public ScheduleRecord getSchedule() {
        return schedule2Failure;
    }

}
