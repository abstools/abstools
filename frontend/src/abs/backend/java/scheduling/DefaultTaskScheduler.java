/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.scheduling;

import java.util.LinkedList;
import java.util.List;
import java.util.logging.Logger;

import abs.backend.java.lib.runtime.ABSGuard;
import abs.backend.java.lib.runtime.ABSRuntime;
import abs.backend.java.lib.runtime.ABSThread;
import abs.backend.java.lib.runtime.ABSThreadManager;
import abs.backend.java.lib.runtime.COG;
import abs.backend.java.lib.runtime.Logging;
import abs.backend.java.lib.runtime.Task;
import abs.backend.java.observing.TaskSchedulerView;
import abs.backend.java.observing.TaskView;

public class DefaultTaskScheduler implements TaskScheduler {
    private static final Logger log = Logging.getLogger(ABSRuntime.class.getName());

    private final List<Task<?>> newTasks = new LinkedList<Task<?>>();
    private final List<SchedulerThread> suspendedTasks = new LinkedList<SchedulerThread>();
    private Task<?> activeTask;
    private volatile SchedulerThread thread;
    private final COG cog;
    private final ABSThreadManager threadManager;

    public DefaultTaskScheduler(COG cog, ABSThreadManager m) {
        this.cog = cog;
        this.threadManager = m;
    }

    @Override
    public synchronized void addTask(Task<?> task) {
        newTasks.add(task);
        if (view != null)
            view.taskAdded(task.getView());
        log.finest(task + " ADDED TO QUEUE");

        if (thread == null) {
            thread = new SchedulerThread();
            thread.start();
        } else {
            notifyAll();
        }
    }

    class SchedulerThread extends ABSThread {
        private Task<?> runningTask;

        public SchedulerThread() {
            super(threadManager);
            setName("ABS Scheduler Thread of " + cog.toString());
            setCOG(cog);
        }

        @Override
        public void run() {
            try {
            loop:
            while (!shutdown) {
                synchronized (DefaultTaskScheduler.this) {
                    activeTask = null;
                    if (newTasks.isEmpty()) {
                        thread = null;
                        DefaultTaskScheduler.this.notifyAll();
                        break loop;
                    }

                    activeTask = newTasks.remove(0);
                    runningTask = activeTask;
                    setName("ABS Scheduler Thread executing " + activeTask.toString());
                }

                View v = view;

                log.finest("Executing " + runningTask);
                try {
                    runningTask.run();
                    v = view;
                    log.finest("Task " + runningTask + " FINISHED");

                } catch (Exception e) {
                    log.finest("EXCEPTION in Task " + runningTask);
                    e.printStackTrace();
                }
            }
            } finally {
                finished();
            }
        }

        // assume called in synchronized block
        public void suspendTask(ABSGuard g) {
            log.finest(runningTask + " on " + g + " SUSPENDING");
            synchronized (DefaultTaskScheduler.this) {
                activeTask = null;
                thread = null;
                if (!newTasks.isEmpty()) {
                    log.finest(runningTask + " on " + g + " Starting new Scheduler Thread");

                    thread = new SchedulerThread();
                    thread.start();
                } else {
                    DefaultTaskScheduler.this.notifyAll();
                }
                log.finest(runningTask + " on " + g + " SUSPENDING");
                suspendedTasks.add(this);
            }

            View v = view;
            if (v != null) {
                v.taskSuspended(runningTask.getView(), g);
            }

            log.finest(runningTask + " AWAITING " + g);
            boolean couldBecomeFalse = g.await();
            if (Thread.interrupted()) {
                return;
            }

            if (!couldBecomeFalse) {
                log.finest(runningTask + " " + g + " READY");
                if (v != null)
                    v.taskReady(runningTask.getView());
            }

            synchronized (DefaultTaskScheduler.this) {
                while (!(g.isTrue() && thread == null)) {
                    try {
                        log.finest(runningTask + " " + g + " WAITING FOR WAKE UP");
                        DefaultTaskScheduler.this.wait();
                        log.finest(runningTask + " WOKE UP...");
                    } catch (InterruptedException e) {
                        e.printStackTrace();
                        break;
                    }
                }
                thread = this;
                activeTask = runningTask;
                suspendedTasks.remove(this);
            }

            if (v != null)
                v.taskResumed(runningTask.getView(), g);

            log.finest(runningTask + " " + g + " ACTIVE");
        }
    }

    @Override
    public void await(ABSGuard g) {
        thread.suspendTask(g);
    }

    @Override
    public synchronized Task<?> getActiveTask() {
        return activeTask;
    }

    private volatile View view;

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
            public TaskScheduler createTaskScheduler(ABSRuntime runtime, COG cog, ABSThreadManager m, ScheduableTasksFilter filter) {
                return new DefaultTaskScheduler(cog, m);
            }
        };
    }

    @Override
    public COG getCOG() {
        return cog;
    }

}
