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
 * <p>
 * This class creates an instance of the `SchedulerThread` inner class for
 * each running task.  Instances of `SchedulerThread` are re-used since they
 * will pick up a fresh task after their current task ends; if no incoming
 * task is waiting, the thread terminates.
 */
public class DefaultTaskScheduler implements TaskScheduler {
    private static final Logger log = Logging.getLogger(DefaultTaskScheduler.class.getName());

    /**
     * Incoming tasks added via addTask.  Each task is consumed and handled by
     * either a fresh `SchedulerThread`, or by a `SchedulerThread` that
     * finished its previous task.
     */
    private final List<Task<?>> newTasks = new LinkedList<>();
    /**
     * The currently executing thread.  This field serves as the cog-wide
     * mutex, ensuring that only one SchedulerThread executes its task.  All
     * access must be protected by `synchronized`.
     */
    private volatile SchedulerThread runningThread;
    private final COG cog;
    private final ABSThreadManager threadManager;

    /**
     * The currently active task, or {@ocde null} if idle.  Note that this
     * field is very much linked to {@code runningThread}, and must be updated
     * at the same time.  We store this as a member because the cog will call
     * {@code getActiveTask} to determine if the current thread has given up
     * the lock.
     */
    private Task<?> activeTask; // TODO: check if we can replace it with runningThread.runningTask
    private volatile View view;

    public DefaultTaskScheduler(COG cog, ABSThreadManager m) {
        this.cog = cog;
        this.threadManager = m;
    }

    @Override
    public synchronized void addTaskToScheduler(Task<?> task) {
        newTasks.add(task);
        if (view != null)
            view.taskAdded(task.getView());
        log.finest(() -> task + " ADDED TO QUEUE");

        if (runningThread == null) {
            // We're idle and/or all threads are suspended waiting:
            // SchedulerThread#init will pick up a task from `newTasks`.
            runningThread = new SchedulerThread();
            runningThread.start();
        } else {
            // Some thread is running, don't start a new thread since the new
            // task will be picked up when the task of the running thread
            // suspends or finishes.
            //
            // TODO: figure out why we wake up all suspended threads here
            notifyAll();
        }
    }

    /**
     * The (Java) thread executing one (ABS) task.
     */
    class SchedulerThread extends ABSThread {
        /**
         * The task currently being handled by this thread instance.
         */
        private Task<?> runningTask;

        public SchedulerThread() {
            super(threadManager);
            setName("ABS Scheduler Thread of " + cog.toString());
            setCOG(cog);
        }

        @Override
        public void run() {
            super.run();
            try {
                // We have:
                // - runningThread (field of scheduler)
                //   - set by scheduler in `addTask`, or by another thread in
                //     suspend
                //   -  cleared by thread when shutting down itself, or by
                //     thread when suspending its task
                // - runningTask (local field of thread)
                //
                // The `loop` loop makes this thread pick up a new task from
                // `newTasks` after finishing our current task; note that
                // `addTask` only creates a fresh thread if `runningThread` is
                // null.
                loop:
                while (!shutdown) {
                    synchronized (DefaultTaskScheduler.this) {
                        DefaultTaskScheduler.this.activeTask = null;
                        if (newTasks.isEmpty()) {
                            DefaultTaskScheduler.this.runningThread = null; // release cog token
                            DefaultTaskScheduler.this.notifyAll();
                            break loop;
                        }

                        DefaultTaskScheduler.this.activeTask = newTasks.remove(0);
                        runningTask = DefaultTaskScheduler.this.activeTask;
                        setName("ABS Scheduler Thread executing " + activeTask.toString());
                    }

                    log.finest(() -> "Executing " + runningTask);
                    try {
                        runningTask.run();
                        cog.notifyEnded();
                        log.finest(() -> "Task " + runningTask + " FINISHED");

                    } catch (Exception e) {
                        log.finest(() -> "EXCEPTION in Task " + runningTask);
                        e.printStackTrace();
                    }
                }
            } finally {
                finished();
            }
        }

        // assume called in synchronized block
        public void suspendTask(ABSGuard g) {
            synchronized (DefaultTaskScheduler.this) {
                DefaultTaskScheduler.this.activeTask = null; // mark inactive: we're not blocking
                DefaultTaskScheduler.this.runningThread = null; // release token
                if (!newTasks.isEmpty()) {
                    // A new method call came in while we were running: create
                    // its thread
                    log.finest(() -> runningTask + " on " + g + " Starting new Scheduler Thread");
                    runningThread = new SchedulerThread();
                    runningThread.start();
                } else {
                    // Start a scheduling round: we already set
                    // `runningThread` to null, so someone else can grab it
                    DefaultTaskScheduler.this.notifyAll();
                }
                log.finest(() -> runningTask + " on " + g + " SUSPENDING");
            }

            View v = view;
            if (v != null) {
                v.taskSuspended(runningTask.getView(), g);
            }

            log.finest(() -> runningTask + " AWAITING " + g);
            boolean taskReady = g.await(cog, runningTask); // Note that this might suspend the thread
            if (Thread.interrupted()) {
                return;
            }
            if (taskReady && g.staysTrue()) {
                log.finest(() -> runningTask + " " + g + " READY");
                if (v != null)
                    v.taskReady(runningTask.getView());
            }

            synchronized (DefaultTaskScheduler.this) {
                while (runningThread != null || !g.await(cog, runningTask)) {
                    // Sleep when someone else is running, or our guard
                    // evalutes to false
                    try {
                        log.finest(() -> runningTask + " " + g + " WAITING FOR WAKE UP");
                        DefaultTaskScheduler.this.wait();
                        log.finest(() -> runningTask + " WOKE UP...");
                    } catch (InterruptedException e) {
                        e.printStackTrace();
                        break;
                    }
                }
                runningThread = this; // grab lock
                activeTask = runningTask; // let scheduler know which task is running
            }

            if (v != null)
                v.taskResumed(runningTask.getView(), g);

            log.finest(() -> runningTask + " " + g + " ACTIVE");
        }

	@Override
	public void checkGuard() { }
    }

    @Override
    public void await(ABSGuard g) {
        runningThread.suspendTask(g);
    }

    @Override
    public synchronized Task<?> getActiveTask() {
        return activeTask;
    }

    @Override
    public synchronized TaskSchedulerView getView() {
        if (view == null) {
            view = new View();
        }
        return view;
    }

    private class View extends AbstractTaskSchedulerView {
        @Override
        public List<TaskView> getReadyTasks() {
            return null;
        }

        @Override
        public List<TaskView> getSuspendedTasks() {
            return null;
        }

        @Override
        public List<TaskView> getSchedulableTasks() {
            return null;
        }

        @Override
        public TaskView getActiveTask() {
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
