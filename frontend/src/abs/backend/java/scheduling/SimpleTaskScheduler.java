/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.scheduling;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Random;
import java.util.concurrent.atomic.AtomicLong;
import java.util.logging.Level;
import java.util.logging.Logger;

import abs.backend.java.lib.runtime.ABSAndGuard;
import abs.backend.java.lib.runtime.ABSFutureGuard;
import abs.backend.java.lib.runtime.ABSGuard;
import abs.backend.java.lib.runtime.ABSInitObjectCall;
import abs.backend.java.lib.runtime.ABSRuntime;
import abs.backend.java.lib.runtime.ABSThread;
import abs.backend.java.lib.runtime.ABSThreadManager;
import abs.backend.java.lib.runtime.COG;
import abs.backend.java.lib.runtime.Config;
import abs.backend.java.lib.runtime.Logging;
import abs.backend.java.lib.runtime.SystemTerminatedException;
import abs.backend.java.lib.runtime.Task;
import abs.backend.java.observing.SystemObserver;
import abs.backend.java.observing.TaskObserver;
import abs.backend.java.observing.TaskSchedulerView;
import abs.backend.java.observing.TaskView;
import abs.backend.java.scheduling.SimpleTaskScheduler.TaskInfo;

/**
 * A simple scheduler that is not the most efficient one, but is easy to
 * understand and also makes it easy to override the scheduling behavior
 *
 * @author Jan Schäfer
 *
 */
public class SimpleTaskScheduler implements TaskScheduler {
    private final AtomicLong idCounter = new AtomicLong();
    static Logger logger = Logging.getLogger("scheduler");
    private final ABSThreadManager threadManager;
    private final ScheduableTasksFilter scheduableTasksFilter;

    public class TaskInfo {
        /**
         * A COG-wide unique id, that is increment for each new task.
         */
        public long id = idCounter.incrementAndGet();

        public final Task<?> task;

        /**
         * is null unless this task as a fixed assigned thread
         */
        public SimpleSchedulerThread thread;

        /**
         * is null unless this task is waiting on a guard suspended tasks are
         * always waiting on a guard
         */
        public ABSGuard guard;

        /**
         * stores whether this task has already been activated once
         */
        public boolean hasBeenActivated = false;

        public TaskInfo(Task<?> task) {
            this.task = task;
        }

        public boolean isSchedulable() {
            return !isSuspended() || guard.isTrue();
        }

        public boolean isSuspended() {
            return guard != null;
        }

        public void makeReady() {
            guard = null;
        }

        public void suspend(ABSGuard g) {
            guard = g;
        }

        public String toString() {
            return "Task " + task.getID();
        }

    }

    protected TaskSchedulingStrategy schedulingStrategy;

    /**
     * Holds a list of all ready tasks. Ready tasks are tasks that can
     * definitely be scheduled. Note that this list does not include tasks that
     * are suspended on an unstable guard, i.e., a guard that may become false
     * after it was true. Such tasks are in the suspendedTasks list.
     *
     * Invariant: for all task in readyTasks: task.isSuspended() == false
     */
    protected final List<TaskInfo> readyTasks = new ArrayList<>();

    /**
     * Holds a list of all suspended tasks Invariant: for all task in
     * suspendedTasks: task.isSuspended() == true
     */
    protected final List<TaskInfo> suspendedTasks = new ArrayList<>();

    /**
     * Holds the currently active task or is null if there is no active task
     */
    protected TaskInfo activeTask;

    protected final COG cog;
    private final ABSRuntime runtime;

    public SimpleTaskScheduler(COG cog, TaskSchedulingStrategy strat, ABSRuntime runtime, ABSThreadManager m, ScheduableTasksFilter filter) {
        this.threadManager = m;
        this.cog = cog;
        if (strat != null) {
            // use user-defined strategy
            this.schedulingStrategy = strat;
        } else if (runtime.getTaskSchedulingStrategy() != null) {
            // use global strategy
            this.schedulingStrategy = runtime.getTaskSchedulingStrategy();
        } else {
            // use random scheduling strategy as a fallback
            this.schedulingStrategy = new RandomSchedulingStrategy(runtime.getRandom());
        }
        logger.config("TaskSchedulingStrategy: " + this.schedulingStrategy.getClass().getName());
        this.runtime = runtime;
        this.scheduableTasksFilter = filter;
    }

    protected void taskDeadlocked() {
        logger.warning("Task "+activeTask+" deadlocked");
        runtime.doNextStep();

    }

    protected void taskFinished() {
        logger.finest("Task finished getting monitor...");
        TaskInfo finishedTask = null;
        synchronized (this) {
            logger.finest("got monitor");
            finishedTask = activeTask;
            finishedTask.task.setFinished(true);
            activeTask = null;
            if (suspendedTasks.size() + readyTasks.size() > 0) {
                logger.finest("calling schedule...");
                schedule();
                logger.finest("schedule called");
            }
        }
        logger.finest("do next step");
        // we now have to wait for all tasks that waited for the future
        // of this task to give them the opportunity to add a schedule action
        // to the global scheduler, before we do this step
        runtime.doNextStep();
        logger.finest("next step done");
    }


    @Override
    public synchronized void addTask(final Task<?> task) {

        readyTasks.add(new TaskInfo(task));
        if (view != null)
            view.taskAdded(task.getView());

        if (activeTask == null) {
            if (runtime.hasGlobalScheduler() &&
                (task.getCall() instanceof ABSInitObjectCall)) {
                    runtime.addScheduleAction(new ActivateTask(cog,task) {
                        @Override
                        public void execute() {
                            synchronized (SimpleTaskScheduler.this) {
                                TaskInfo ti = readyTasks.remove(0);
                                activateTask(ti);
                            }
                        }
                    });
            } else {
                schedule();
            }
        }
    }

    class SimpleSchedulerThread extends ABSThread implements GuardWaiter {

        private final TaskInfo executingTask;

        private boolean active;

        private ABSGuard guard;

        public SimpleSchedulerThread(TaskInfo activeTask) {
            super(threadManager);
            this.executingTask = activeTask;
            setName("ABS Scheduler Thread of " + cog.toString());
            setCOG(cog);
        }

        public TaskInfo getExecutingTask() {
            return executingTask;
        }

        public List<Task<?>> getSuspendedTasks() {
            List<Task<?>> result = new ArrayList<>();
            for (TaskInfo ti : suspendedTasks) {
                result.add(ti.task);
            }
            return result;
        }

        @Override
        public void run() {
            try {
                active = true;
                executingTask.task.run();
                if (executingTask.task.isDeadlocked())
                    taskDeadlocked();
                else
                    taskFinished();
            } catch (SystemTerminatedException e){

            } finally {
                finished();
            }
        }

        public void checkGuard() {
            // have to take locks in that order to prevent deadlocks
            // because schedule might get called
            synchronized (SimpleTaskScheduler.this) {
                synchronized (this) {
                    logger.finest(executingTask + " checking guard");
                    if (guard.isTrue() && guard.staysTrue()) {
                        logger.finest(executingTask + " got monitor");
                        suspendedTasks.remove(executingTask);
                        readyTasks.add(executingTask);
                        executingTask.makeReady();
                        if (view != null)
                            view.taskReady(executingTask.task.getView());
                        if (activeTask == null) {
                            logger.finest(executingTask + " scheduling myself");
                            schedule();
                        }
                    }
                }
            }
        }

        synchronized void setGuard(ABSGuard g) {
            logger.finest(executingTask + " awaiting " + g);
            active = false;
            this.guard = g;

            logger.finest(executingTask + " registering at threads...");
            boolean wasAdded = registerAtThreads(g);

            if (!wasAdded) {
                logger.fine(this+" was not added to guard "+g);
            }
        }

        void await(ABSGuard g) {

            logger.finest(executingTask + " next step done going into monitor");
            synchronized (this) {
                try {
                    logger.finest(executingTask + " waiting to be resumed");
                    while (!active) {
                        wait();
                    }
                } catch (InterruptedException e) {
                    wasInterrupted(e);
                }
                logger.finest(executingTask + " resumed");
                active = true;
            }
        }

        private boolean registerAtThreads(ABSGuard g) {
            boolean wasAdded = false;
            if (g instanceof ABSFutureGuard) {
                ABSFutureGuard fg = (ABSFutureGuard) g;
                wasAdded = fg.fut.addWaitingThread(this);
                logger.finest(executingTask + " was "+(wasAdded ? "" :"NOT ")+"added to " + fg.fut);
            } else if (g instanceof ABSAndGuard) {
                ABSAndGuard ag = (ABSAndGuard) g;
                wasAdded = registerAtThreads(ag.getLeftGuard());
                wasAdded |= registerAtThreads(ag.getRightGuard());
            }
            return wasAdded;
        }

        public synchronized void awake() {
            active = true;
            notify();
            logger.fine(executingTask.toString() + " awaked");
        }

    }

    private synchronized void schedule() {
        if (runtime.hasGlobalScheduler()) {
            if (suspendedTasks.isEmpty() && readyTasks.isEmpty())
                return;
            logger.finest("Adding scheduling action...");
            runtime.addScheduleAction(new ScheduleTask(cog) {
                @Override
                public void execute() {
                    logger.finest("Calling do schedule");
                    doSchedule();
                }

            });
            logger.finest("Done");
        } else {
            doSchedule();
        }
    }

    private void doSchedule() {
        logger.finest("Executing doSchedule...");

        List<TaskInfo> choices = getSchedulableTasks();

        if (logger.isLoggable(Level.INFO))
            logger.info("COG " + cog.getID() + " scheduling choices: " + choices);

        if (choices.isEmpty()) {
            logger.info("Choices are empty!");
            runtime.doNextStep();
            return;
        }

        TaskInfo nextTask = schedule(choices);
        nextTask.hasBeenActivated = true;
        synchronized (this) {

            if (nextTask.isSuspended()) {
                suspendedTasks.remove(nextTask);
                nextTask.makeReady();
            } else {
                readyTasks.remove(nextTask);
            }

            activateTask(nextTask);
        }

    }

    private synchronized List<TaskInfo> getSchedulableTasks() {
        List<TaskInfo> suspendedTasksWithSatisfiedGuards = unsuspendTasks();
        List<TaskInfo> choices = new ArrayList<>(readyTasks);
        choices.addAll(suspendedTasksWithSatisfiedGuards);
        List<TaskInfo> choicesFiltered = scheduableTasksFilter.filter(choices);
        return choicesFiltered;
    }

    private synchronized void activateTask(TaskInfo nextTask) {
        activeTask = nextTask;
        if (activeTask.thread != null) {
            logger.info("COG " + cog.getID() + " awaking " + activeTask);
            activeTask.thread.awake();
        } else {
            logger.info("COG " + cog.getID() + " creating " + activeTask);
            activeTask.thread = new SimpleSchedulerThread(activeTask);
            activeTask.thread.start();
            activeTask.task.setStart(System.currentTimeMillis());
        }
    }

    private synchronized List<TaskInfo> unsuspendTasks() {
        List<TaskInfo> tasksWithSatisfiedGuards = new ArrayList<>(0);

        Iterator<TaskInfo> it = suspendedTasks.iterator();
        while (it.hasNext()) {
            TaskInfo task = it.next();
            if (task.guard.isTrue()) {
                if (task.guard.staysTrue()) {
                    readyTasks.add(task);
                    task.makeReady();
                    it.remove();
                } else {
                    tasksWithSatisfiedGuards.add(task);
                }
            }
        }
        return tasksWithSatisfiedGuards;
    }

    protected TaskInfo schedule(List<TaskInfo> schedulableTasks) {
        return schedulingStrategy.schedule(this, schedulableTasks);
    }

    private volatile View view;

    private final Object viewCreatorLock = new Object();

    @Override
    public TaskSchedulerView getView() {
        synchronized (viewCreatorLock) {
        if (view == null)
            view = new View();
        return view;
        }
    }

    @Override
    public synchronized Task<?> getActiveTask() {
        if (activeTask == null)
            return null;
        return activeTask.task;
    }

    @Override
    public void await(ABSGuard g) {
        SimpleSchedulerThread thread = null;
        TaskInfo newTask = null;
        TaskInfo currentTask = null;

        synchronized (this) {
            if (activeTask == null) {
                // while shutting down the activeTask might bee null
                return;
            }
            currentTask = activeTask;
            thread = currentTask.thread;
            currentTask.suspend(g);
            suspendedTasks.add(currentTask);
            activeTask = null;
            if (view != null)
                view.taskSuspended(currentTask.task.getView(), g);

            thread.setGuard(g);

            if (g.isTrue() || (suspendedTasks.size() + readyTasks.size()) > 1) {
                logger.fine("issuing a schedule");
                schedule();
            }

        }


        runtime.doNextStep();

        synchronized (this) {
            newTask = activeTask;
        }

        if (newTask != currentTask) {
            thread.await(g);
        }

        if (view != null)
            view.taskResumed(currentTask.task.getView(), g);

    }

    public static TaskSchedulerFactory getFactory() {
        return new TaskSchedulerFactory() {
            @Override
            public TaskScheduler createTaskScheduler(ABSRuntime runtime, COG cog, ABSThreadManager m, ScheduableTasksFilter filter) {
                return new SimpleTaskScheduler(cog, null, runtime, m, filter);
            }
        };
    }

    @Override
    public COG getCOG() {
        return cog;
    }

    public TaskSchedulingStrategy getSchedulingStrategy() {
        return schedulingStrategy;
    }

    // Enable changing the task scheduling strategy at runtime
    public void setSchedulingStrategy(TaskSchedulingStrategy strat) {
        schedulingStrategy = strat;
    }

    private class View extends AbstractTaskSchedulerView {

        @Override
        public List<TaskView> getSchedulableTasks() {
            synchronized (SimpleTaskScheduler.this) {
                ArrayList<TaskView> result = new ArrayList<>();
                if (getActiveTask() != null) {
                    result.add(getActiveTask());
                    return result;
                }

                for (TaskInfo t : SimpleTaskScheduler.this.getSchedulableTasks()) {
                    result.add(t.task.getView());
                }

                return result;
            }
        }

        @Override
        public List<TaskView> getReadyTasks() {
            synchronized (SimpleTaskScheduler.this) {
                ArrayList<TaskView> result = new ArrayList<>();
                for (TaskInfo t : readyTasks) {
                    result.add(t.task.getView());
                }

                return result;
            }
        }

        @Override
        public List<TaskView> getSuspendedTasks() {
            synchronized (SimpleTaskScheduler.this) {
                ArrayList<TaskView> result = new ArrayList<>();
                for (TaskInfo t : suspendedTasks) {
                    result.add(t.task.getView());
                }
                return result;
            }

        }

        @Override
        public TaskView getActiveTask() {
            Task<?> activeTask = SimpleTaskScheduler.this.getActiveTask();
            if (activeTask == null)
                return null;
            return activeTask.getView();
        }

    }

}
