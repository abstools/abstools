/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.observing;

import java.util.List;

public interface TaskSchedulerView {

    /**
     * Returns a list of all ready tasks. Ready tasks are tasks that can
     * definitely be scheduled. Note that this list does not include tasks that
     * are suspended on an unstable guard, i.e., a guard that may become false
     * after it was true. Such tasks are in the suspendedTasks list.
     */
    List<TaskView> getReadyTasks();

    /**
     * Returns a list of all suspended tasks.
     * All these tasks wait on a guard.
     * Some of these guards may be true.
     */
    List<TaskView> getSuspendedTasks();

    /**
     * Returns a list of schedulable tasks.
     * This list contains all tasks of getReadyTasks() plus the tasks
     * from getSuspendedTasks(), which are waiting on a guard which is true
     */
    List<TaskView> getSchedulableTasks();
    
    TaskView getActiveTask();

    void registerTaskSchedulerObserver(TaskSchedulerObserver listener);

}
