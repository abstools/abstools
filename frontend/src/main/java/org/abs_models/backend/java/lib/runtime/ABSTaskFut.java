/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.lib.runtime;

import org.abs_models.backend.java.lib.types.ABSValue;
import org.abs_models.backend.java.observing.TaskView;

/**
 * Future implementation that depends on a task

 * @author Jan Schäfer
 *
 * @param <V>
 */
public class ABSTaskFut<V extends ABSValue> extends ABSFut<V> {
    private final Task<?> resolvingTask;


    public ABSTaskFut(Task<?> task) {
        resolvingTask = task;
    }



    @Override
    public V get() {
        synchronized (this) {
            if (!isDone() && resolvingTask.getCOG() == ABSThread.getCurrentCOG())
                throw new ABSDeadlockException();
        }

        final Task<?> t = ABSThread.getCurrentTask();
        if (t != null) {
            t.calledGetOnFut(this);
        }

        if (ABSRuntime.getRuntime().hasGlobalScheduler()) {
            ABSRuntime.getRuntime().getGlobalScheduler().handleGet(this);
        }

        await(ABSThread.getCurrentCOG(), ABSThread.getCurrentTask());
        log.finest(() -> "future " + this + " awaited");

        if (t != null) {
            t.futureReady(this);
        }

        log.finest(() -> this + " continue after get");

        if (exception != null)
            throw exception;
        return value;
    }

    @Override
    public synchronized String toString() {
        return "Future of " + resolvingTask + " (" + (isDone ? value : "unresolved") + ")";
    }

    public Task<?> getResolvingTask() {
        return resolvingTask;
    }

    @Override
    protected View createView() {
        return new TaskFutView();
    }

    private class TaskFutView extends View {
        @Override
        public TaskView getResolvingTask() {
            return ABSTaskFut.this.resolvingTask.getView();
        }
    }

}
