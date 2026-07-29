/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.scheduling;

import java.util.LinkedList;
import java.util.List;
import java.util.logging.Logger;

import org.abs_models.backend.java.lib.runtime.ABSGuard;
import org.abs_models.backend.java.lib.runtime.ABSRuntime;
import org.abs_models.backend.java.lib.runtime.ABSThread;
import org.abs_models.backend.java.lib.runtime.ABSThreadManager;
import org.abs_models.backend.java.lib.runtime.COG;
import org.abs_models.backend.java.lib.runtime.Logging;
import org.abs_models.backend.java.lib.runtime.Task;
import org.abs_models.backend.java.observing.TaskSchedulerView;
import org.abs_models.backend.java.observing.TaskView;

/**
 * The default task scheduler class.  Each cog has a `TaskScheduler` instance
 * that it delegates task scheduling to; this is the default class.
 *
 * <p>This class creates an instance of the `SchedulerThread` inner
 * class for each running task.
 */
public class DefaultTaskScheduler implements TaskScheduler {
    private static final Logger log = Logging.getLogger(DefaultTaskScheduler.class.getName());

    /**
     * The currently executing thread.  This field serves as the
     * cog-wide mutex, ensuring that only one SchedulerThread executes
     * its task.  All access must be protected by `synchronized`.
     * Rule: whoever sets this to null must call notifyAll() as well.
     */
    private SchedulerThread runningThread;
    private final COG cog;
    private final ABSThreadManager threadManager;

    private volatile View view;

    public DefaultTaskScheduler(COG cog, ABSThreadManager m) {
        this.cog = cog;
        this.threadManager = m;
    }

    @Override
    public synchronized void addTaskToScheduler(Task<?> task) {
        if (view != null)
            view.taskAdded(task.getView());
        log.finest(() -> "STARTING THREAD FOR " + task);
        // If runningThread is null, the new thread will start;
        // otherwise it will wait and get a signal when the current
        // thread suspends or shuts down
        SchedulerThread thread = new SchedulerThread(task);
        Thread.ofVirtual().start(thread);
    }

    /**
     * The (Java virtual) thread executing one (ABS) task.
     */
    class SchedulerThread extends ABSThread {
        /** The task being handled by this thread instance. */
        private Task<?> myTask;

        public Task<?> getTask() { return myTask; }

        public SchedulerThread(Task task) {
            super(threadManager);
            Thread.currentThread().setName("ABS Scheduler Thread of " + cog.toString());
            setCOG(cog);
            myTask = task;
        }

        @Override
        public void run() {
            super.run();
            Thread.currentThread().setName("ABS Scheduler Thread executing " + myTask.toString());
            try {
                acquireLock(null);
                log.finest(() -> "Executing " + myTask);
                try {
                    myTask.run();
                } catch (Exception e) {
                    log.finest(() -> "EXCEPTION in Task " + myTask);
                    e.printStackTrace();
                }
            } finally {
                finished();
                releaseLockAndSignal();
                cog.notifyEnded(myTask);
                log.finest(() -> "Task " + myTask + " FINISHED");
            }
        }

        /**
         * Acquire the scheduler lock: loop until runningThread is
         * null, then set it to ourselves and return.  If we await on
         * a guard, we assume we're only called if the guard evaluates
         * to true (but could be non-monotonic).
         *
         * @param g The guard that is waited on
         */
        private void acquireLock(ABSGuard g) {
            assert myTask == g.getTask() : "Trying to await on a guard that is not from the current task";
            if (g == null || g.staysTrue()) {
                synchronized (DefaultTaskScheduler.this) {
                    log.finest(() -> myTask + " ENTERING ACQUIRE LOOP");
                    while (DefaultTaskScheduler.this.runningThread != null) {
                        // Sleep when someone else is running
                        try {
                            log.finest(() -> myTask + " WAITING FOR TOKEN");
                            DefaultTaskScheduler.this.wait();
                            log.finest(() -> myTask + " GOT SIGNAL");
                        } catch (InterruptedException e) {
                            e.printStackTrace();
                        }
                    }
                    DefaultTaskScheduler.this.runningThread = this; // grab lock
                }
                log.finest(() -> myTask + " GOT TOKEN");
            } else {
                synchronized(DefaultTaskScheduler.this) {
                    log.finest(() -> myTask + " ENTERING ACQUIRE LOOP WITH GUARD");
                    // NOTE: if g.await() blocks, directly or
                    // indirectly, this can lead to deadlocks.
                    while (DefaultTaskScheduler.this.runningThread != null || !g.await()) {
                        // Sleep when someone else is running, or g not ready
                        try {
                            log.finest(() -> myTask + " WAITING FOR TOKEN");
                            DefaultTaskScheduler.this.wait();
                            log.finest(() -> myTask + " GOT SIGNAL");
                        } catch (InterruptedException e) {
                            e.printStackTrace();
                        }
                    }
                    DefaultTaskScheduler.this.runningThread = this; // grab lock
                }
                log.finest(() -> myTask + " GOT TOKEN");
            }
        }

        /**
         * Release the scheduler lock and wake up other waiting
         * threads.
         */
        private void releaseLockAndSignal() {
            log.finest(() -> "Task " + myTask + " PREPARING TO RELASE TOKEN");
            synchronized(DefaultTaskScheduler.this) {
                log.finest(() -> "Task " + myTask + " RELEASING TOKEN");
                DefaultTaskScheduler.this.runningThread = null;
                DefaultTaskScheduler.this.notifyAll();
            }
        }

        /**
         * Release token, wait until guard is true, then acquire token
         * again.  Slightly complicated by the fact that
         * ExpressionGuard is non-monotonic and can become false
         * again...
         */
        public void suspendTask(ABSGuard g) {
            assert myTask == g.getTask() : "Trying to await on a guard that is not from the current task";
            log.finest(() -> myTask + " on " + g + " SUSPENDING");
            releaseLockAndSignal();
            View v = view;
            if (v != null) {
                v.taskSuspended(myTask.getView(), g);
            }
            log.finest(() -> myTask + " AWAITING " + g);
            boolean taskReady = g.await(); // Note that this might suspend the thread again
            log.finest(() -> myTask + " " + g + " READY");
            if (v != null)
                v.taskReady(myTask.getView()); // ignoring non-monitonic tasks here ...
            acquireLock(g);
            if (v != null)
                v.taskResumed(myTask.getView(), g);
            log.finest(() -> myTask + " " + g + " ACTIVE");
        }

        @Override
        public void checkGuard() { }
    }

    @Override
    public void await(ABSGuard g) {
        runningThread.suspendTask(g);
    }

    @Override
    public Task<?> getActiveTask() {
        // NOTE: this method is not synchronized, since it is called
        // by ExpGuard.await, which is called by
        // SchedulerThread.acquireLock inside a synchronized block
        SchedulerThread _runningThread = runningThread;
        if (_runningThread == null) return null;
        else return _runningThread.getTask();
    }

    @Override
    public TaskSchedulerView getView() {
        if (view == null) {
            synchronized(this) {
                if (view == null) {
                    view = new View();
                }
            }
        }
        return view;
    }

    private class View extends AbstractTaskSchedulerView {
        @Override
        public List<TaskView> getReadyTaskViews() {
            return null;
        }

        @Override
        public List<TaskView> getSuspendedTaskViews() {
            return null;
        }

        @Override
        public List<TaskView> getSchedulableTaskViews() {
            return null;
        }

        @Override
        public TaskView getActiveTaskView() {
            return DefaultTaskScheduler.this.getActiveTask().getView();
        }

    }

    public static TaskSchedulerFactory getFactory() {
        return new TaskSchedulerFactory() {
            @Override
            public TaskScheduler createTaskScheduler(ABSRuntime runtime, COG cog, ABSThreadManager m, SchedulableTasksFilter filter) {
                return new DefaultTaskScheduler(cog, m);
            }
        };
    }

    @Override
    public COG getCOG() {
        return cog;
    }

}
