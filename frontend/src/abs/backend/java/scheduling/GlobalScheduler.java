/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.scheduling;

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

    public GlobalScheduler(ABSRuntime runtime, GlobalSchedulingStrategy strategy) {
        this.strategy = strategy;
        this.runtime = runtime;
    }

    private long totalNumChoices = 0;

    public void doNextScheduleStep() {
        if (isShutdown) return;
        logger.finest("Do next step...");
        ScheduleAction next = null;
        synchronized (this) {
            if (ignoreNextStep) {
                ignoreNextStep = false;
                logger.finest("Ignored step");
                notify();
                return;
            }
            if (options.isEmpty()) {
                System.out.println("No steps left. Program finished");
                System.out.println("Total number of global choices: " + totalNumChoices);
                if (totalNumChoices == 0) {
                    System.out.println("Program is deterministic!");
                }
                return;
            }

            totalNumChoices += options.numOptions() - 1;

            logger.finest("Choose next action...");
            next = strategy.choose(options);
            logger.finest("Action " + next + " choosen");
            options.removeOption(next);
            logger.finest("Executing Action " + next);


        }
        if (isShutdown) return;
        next.execute();
        logger.finest("Action " + next + " was executed.");
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

    public synchronized void ignoreNextStep() {
        ignoreNextStep = true;
    }

    public synchronized void awaitNextStep() {
        while (ignoreNextStep) {
            try {
                logger.finest("Awaiting next step...");
                wait();
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }
        logger.finest("Next step done");
    }

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
        System.out.println("future waiting");
        w.await();

    }

    static class Waker implements GuardWaiter {
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
                }

            }
            if (Logging.DEBUG)
                logger.finest("task awaked");
        }

        @Override
        public void checkGuard() {
            if (Logging.DEBUG)
                logger.finest("checking guard...");

            globalScheduler.ignoreNextStep();
            awake();
            if (Logging.DEBUG)
                logger.finest("await next step");
            globalScheduler.awaitNextStep();
        }
    }

    public void shutdown() {
        isShutdown = true;
    }

}
