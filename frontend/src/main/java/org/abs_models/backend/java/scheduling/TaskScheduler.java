/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.scheduling;

import org.abs_models.backend.java.lib.runtime.ABSGuard;
import org.abs_models.backend.java.lib.runtime.COG;
import org.abs_models.backend.java.lib.runtime.Task;
import org.abs_models.backend.java.observing.TaskSchedulerView;

/**
 * A TaskScheduler is responsible for executing the tasks of a single COG A task
 * is executed by calling its <code>run</code> method.
 * 
 * Note that the task schedule is in general accessed in a multi-threaded way,
 * so ensure that the implementation is thread-safe
 * 
 * @author Jan Schäfer
 * 
 */
public interface TaskScheduler {


    /**
     * Adds a task to the task queue of this task scheduler
     * 
     * @param task the task to be added
     */
    void addTask(Task<?> task);

    TaskSchedulerView getView();

    /**
     * Returns the currently executing task.
     * 
     * @return null if no task is executing
     */
    Task<?> getActiveTask();

    /**
     * Called by the thread that is executing the currently active task, when
     * hitting an await statement with a guard.
     * 
     * @param g the guard
     */
    void await(ABSGuard g);

    /**
     * The COG of this scheduler
     * @return the COG of this scheduler
     */
    COG getCOG();

}
