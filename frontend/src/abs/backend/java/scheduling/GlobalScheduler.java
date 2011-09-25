/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.scheduling;

import java.util.concurrent.atomic.AtomicInteger;
import java.util.logging.Logger;

import abs.backend.java.lib.runtime.ABSFut;
import abs.backend.java.lib.runtime.ABSRuntime;
import abs.backend.java.lib.runtime.Logging;
import abs.backend.java.lib.runtime.Task;

public class GlobalScheduler {
    private static Logger logger = Logging.getLogger(GlobalScheduler.class.getName());
    private final ScheduleOptions options = new ScheduleOptions();
    private final GlobalSchedulingStrategy strategy;
    private final ABSRuntime runtime;
    private volatile boolean isShutdown;
    private final AtomicInteger counter = new AtomicInteger();

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
            if (ignoreNextStep) {
                ignoreNextStep = false;
                logger.finest("==="+i+": Ignored step");
                notify();
                return;
            }
            if (options.isEmpty()) {
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
        next.execute();
        int j = counter.intValue();
        if (i != j)
            logger.fine("#### Interleaving detected "+i+" != "+j);
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

    private boolean ignoreNextStep;

    private synchronized void ignoreNextStep() {
        ignoreNextStep = true;
    }

    /**
     * Wait until the next step that should be ignored
     * has been executed
     */
    private synchronized void awaitIgnoredStep() {
        while (ignoreNextStep) {
            try {
                logger.finest("Awaiting next step...");
                wait();
            } catch (InterruptedException e) {
                logger.fine("was interrupted");
                Thread.currentThread().interrupt();
                break;
            }
        }
        logger.finest("Next step done");
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
        boolean awaked;
        final GlobalScheduler globalScheduler;
        public Waker(GlobalScheduler scheduler) {
            globalScheduler = scheduler;
        }
        
        
        public synchronized void awake() {
            awaked = true;
            notify();
        }

        public synchronized void await() {
            while (!awaked) {
                try {
                    wait();
                } catch (InterruptedException e) {
                    if (Logging.DEBUG)
                        logger.fine("receveid interrupt exception");
                    Thread.currentThread().interrupt();
                    break;
                }

            }
            if (Logging.DEBUG)
                logger.finest("task awaked");
        }

        @Override
        public void checkGuard() {
            if (Logging.DEBUG)
                logger.finest("checking guard...");

            // the awaked thread will do a call to
            // doNextScheduleStep, however, we have
            // to ignore this call as this thread will 
            // do the call already
            globalScheduler.ignoreNextStep();
            
            awake();
            
            if (Logging.DEBUG)
                logger.finest("await next step");

            // we are now waiting for the awaked thread to do the
            // call to doNextScheduleStep, so that there are no
            // two parallel threads running
            globalScheduler.awaitIgnoredStep();
        }
    }

    public void shutdown() {
        isShutdown = true;
    }

}
