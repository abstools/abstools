/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.scheduling;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.logging.Logger;

import abs.backend.java.lib.runtime.ABSDeadlockException;
import abs.backend.java.lib.runtime.ABSException;
import abs.backend.java.lib.runtime.ABSFut;
import abs.backend.java.lib.runtime.ABSRuntime;
import abs.backend.java.lib.runtime.Logging;
import abs.backend.java.lib.runtime.Task;
import abs.backend.java.scheduling.SimpleTaskScheduler.SimpleSchedulerThread;

public class GlobalScheduler {
    private static Logger logger = Logging.getLogger(GlobalScheduler.class.getName());
    private final ScheduleOptions options = new ScheduleOptions();
    private final GlobalSchedulingStrategy strategy;
    private final ABSRuntime runtime;
    private volatile boolean isShutdown;
    private final AtomicInteger counter = new AtomicInteger();

    /**
     * Holds a list of threads that wait for other threads to complete
     * their next step
     * protected by lock of this
     */
    private final ArrayList<SimpleLock> nextStepWaitStack = new ArrayList<SimpleLock>();

    public GlobalScheduler(ABSRuntime runtime, GlobalSchedulingStrategy strategy) {
        this.strategy = strategy;
        this.runtime = runtime;
    }

    private long totalNumChoices = 0;

    public void doNextScheduleStep() {
        if (isShutdown) return;
        int i = counter.incrementAndGet();
        logger.finest("==="+i+": Do next step...");
        ScheduleAction next = null;
        synchronized (this) {
            if (nextStepWaitStack.size() > 0) {
                SimpleLock l = nextStepWaitStack.remove(nextStepWaitStack.size()-1);
                logger.finest("==="+i+": Ignored step, awaking thread ");
                l.unlock();
                return;
            }
            if (options.isEmpty()) {
                List<SimpleSchedulerThread> activeThreads = runtime.getThreadManager().getAllCopyOf(SimpleSchedulerThread.class);
                if (!activeThreads.isEmpty()) {
                    Set<Task<?>> suspendedTasks = new HashSet<Task<?>>();
                    for (SimpleSchedulerThread st : activeThreads) {
                        Task<?> t = st.getExecutingTask().task;
                        if (t != null && !t.isFinished()) {
                            suspendedTasks.add(t);
                        }
                    }
                    // Need to filter out currentTask (that is finishing)
                    Thread tt = Thread.currentThread();
                    if (tt instanceof SimpleSchedulerThread) {
                        Task<?> currT = ((SimpleSchedulerThread)tt).getExecutingTask().task;
                        suspendedTasks.remove(currT);
                    }
                    if (!suspendedTasks.isEmpty()) {
                        ABSException ex = new ABSDeadlockException();
                        for (Task<?> t : suspendedTasks) {
                            t.setException(ex);
                        }
                        runtime.handleABSException(suspendedTasks.iterator().next(), ex);
                        return;
                    }
                }

                logger.info("No steps left. Program finished");
                logger.info("Total number of global choices: " + totalNumChoices);
                if (totalNumChoices == 0) {
                    logger.info("Program is deterministic!");
                }
                return;
            }

            totalNumChoices += options.numOptions() - 1;

            logger.finest("==="+i+" Choose next action...");
            next = strategy.choose(options);
            logger.finest("==="+i+" Action " + next + " choosen");
            options.removeOption(next);
            logger.finest("==="+i+" Executing Action " + next);


        }
        if (isShutdown) return;
        int j = counter.intValue();
        if (i != j)
            logger.warning("#### Interleaving detected "+i+" != "+j);
        next.execute();
        logger.finest("==="+i+" Action " + next + " was executed.");
    }

    public void stepTask(Task<?> task) throws InterruptedException {
        if (isShutdown) return;

        ScheduleAction a = new StepTask(task);

        synchronized (this) {
            options.addOption(a);
            doNextScheduleStep();
        }

        a.await();
    }

    public synchronized void addAction(ScheduleAction action) {
        options.addOption(action);
    }

    private synchronized void ignoreNextStep(SimpleLock l) {
        nextStepWaitStack.add(l);
    }

    static class SimpleLock {
        private boolean locked;
        synchronized void unlock() {
            locked = false;
            notifyAll();
        }

        synchronized void awaitUnlocked() {
            while (locked) {
                try {
                    logger.finest("Awaiting next step...");
                    wait();
                } catch (InterruptedException e) {
                    logger.fine("was interrupted");
                    Thread.currentThread().interrupt();
                    break;
                }
            }
        }

    }

    /**
     * Handling a get in the global scheduler is pretty tricky.
     * The following is going on here:
     * the current task does a .get on the given future.
     * 2 Cases:
     *
     * 1. Future is ready, that is easy, we just continue
     *
     * 2. Future is not ready
     *
     * this case is difficult, because we have to
     *  1. block the current thread
     *  2. schedule some other task that is ready by choosing one by the
     *     global scheduler
     *  3. when the future is resolved the blocked thread must be awaked,
     *     BUT we must ensure that only one thread is calling the
     *     doNextScheduleStep method.
     *     For this reason there is an ignoreNextStep field that
     *     can be set to ignore the next call to doNextScheduleStep
     *
     *
     *  what we do now is to create a Waker object
     *  this waker object is added to the future we are waiting for, so
     *  that this future can inform us when it is resolved.
     *  we then schedule the next task by calling runtime.doNextStep()
     *  and after that we suspend the thread to be awaked later by the
     *  future, see Waker class for further docu
     *
     * @param fut
     */
    public void handleGet(ABSFut<?> fut) {
        // note that this code does only work in the presence of global
        // scheduling,
        // otherwise it would not be thread-safe
        if (fut.isResolved()) {
            return;
        }

        Waker w = new Waker(this);
        fut.addWaitingThread(w);
        runtime.doNextStep();
        logger.finest("future waiting");
        w.await();

    }

    private static class Waker implements GuardWaiter {
        boolean awaken;
        final GlobalScheduler globalScheduler;
        public Waker(GlobalScheduler scheduler) {
            globalScheduler = scheduler;
        }


        public synchronized void awake() {
            awaken = true;
            notify();
        }

        public synchronized void await() {
            while (!awaken) {
                try {
                    wait();
                } catch (InterruptedException e) {
                    logger.fine("received interrupt exception");
                    Thread.currentThread().interrupt();
                    break;
                }

            }
            logger.finest("task awaked");
        }

        @Override
        public void checkGuard() {
            logger.finest("checking guard...");

            SimpleLock l = new SimpleLock();
            globalScheduler.ignoreNextStep(l);

            awake();

            logger.finest("await next step");

            // we are now waiting for the awaked thread to do the
            // call to doNextScheduleStep, so that there are no
            // two parallel threads running
            l.awaitUnlocked();
        }
    }

    public void shutdown() {
        isShutdown = true;
    }

}
