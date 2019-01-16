/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.observing;

public interface TaskSchedulerObserver {
    void taskCreated(TaskView task);
    void taskReady(TaskView view);
    void taskResumed(TaskView runningTask, GuardView view);
    void taskSuspended(TaskView task, GuardView guard);
}
