/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.scheduling;

import java.util.ArrayList;
import java.util.List;

import org.abs_models.backend.java.lib.runtime.ABSGuard;
import org.abs_models.backend.java.observing.TaskSchedulerObserver;
import org.abs_models.backend.java.observing.TaskSchedulerView;
import org.abs_models.backend.java.observing.TaskView;

public abstract class AbstractTaskSchedulerView implements TaskSchedulerView {

    private List<TaskSchedulerObserver> observers;

    @Override
    public synchronized void registerTaskSchedulerObserver(TaskSchedulerObserver listener) {
        getObservers().add(listener);
    }

    private synchronized List<TaskSchedulerObserver> getObservers() {
        if (observers == null) {
            observers = new ArrayList<>(1);
        }
        return observers;
    }

    public void taskResumed(TaskView runningTask, ABSGuard g) {
        for (TaskSchedulerObserver l : getObserverCopy()) {
            l.taskResumed(runningTask, g.getView());
        }
    }

    public void taskSuspended(TaskView runningTask, ABSGuard g) {
        for (TaskSchedulerObserver l : getObserverCopy()) {
            l.taskSuspended(runningTask, g.getView());
        }
    }

    public void taskAdded(TaskView view) {
        for (TaskSchedulerObserver l : getObserverCopy()) {
            l.taskCreated(view);
        }
    }

    public void taskReady(TaskView view) {
        for (TaskSchedulerObserver l : getObserverCopy()) {
            l.taskReady(view);
        }
    }

    public synchronized List<TaskSchedulerObserver> getObserverCopy() {
        return new ArrayList<>(observers);
    }

}
