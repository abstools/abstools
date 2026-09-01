/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.lib.runtime;

import org.abs_models.backend.java.observing.FutView;
import org.abs_models.backend.java.observing.GuardView;

import org.apfloat.Aprational;

/**
 * The base class of all guards.
 *
 * <p>NOTE: A guard object can only be created in the context of the
 * task in which it will be evaluated: the guard constructor takes a
 * cog as argument, and stores the cog and the cog's current task.
 * This is the context in which the {@code await} method is evaluated,
 * i.e., a guard object is tied to its cog and task.
 */
public abstract class ABSGuard implements GuardView {

    /// The cog that this guard is waiting on.
    final COG cog;
    /// The task that created / runs this task.
    final Task<?> task;

    public ABSGuard(COG cog) {
        this.cog = cog;
        this.task = cog.getScheduler().getActiveTask();
    }

    /// The cog of the task that this guard is waiting on.
    public COG getCog() {
        return cog;
    }

    /// The task that this guard is waiting on.
    public Task<?> getTask() {
        return task;
    }

    /**
     * Return whether the guard is true; i.e., whether the process
     * waiting on the guard can be scheduled.
     *
     * <p>NOTE: It is important to know whether to call this method or
     * the {@link #await} method.  The await method informs the cog
     * about the task's status, so should be called when the result
     * will lead to a scheduling decision.  The {@code isTrue} method
     * should be called when we need to display the guard's status in
     * some way, e.g. for pretty-printing or debugging purposes.
     *
     * <p>To obey this protocol is most important for guards of type
     * {@link ABSExpGuard}, since that guard changes its value
     * frequently, and the runtime must be informed about its
     * effective status at all times.
     *
     * @return the guard's status.
     */
    public abstract boolean isTrue();

    /**
     * Check if the guard is monotonic, i.e., if its state can oscillate
     * between false and true or not.
     *
     * @return true if guard is monotonic, false if not.
     */
    public abstract boolean staysTrue();

    /**
     * Wait and/or check if the guard is true.
     *
     * <p>This method is allowed to suspend the thread, but should arrange to be
     * woken up, e.g., by the future becoming available.  This method will be
     * called multiple times, hence should not do too much work once the guard
     * is true.
     *
     * <p>All implementations of this method must inform the cog about their
     * status via the methods {@link COG#notifyAwait} and {@link
     * COG#notifyWakeup}.
     *
     * @param cog the cog of the task that might be suspended.
     * @param task the task to be suspended.
     * @return the status of the guard (true if runnable, false if not).
     */
    public abstract boolean await();

    public GuardView getView() {
        return this;
    }

    public boolean isExpressionGuard() {
        return this instanceof ABSExpGuard;
    }

    public boolean isFutureGuard() {
        return this instanceof ABSFutureGuard;
    }

    public boolean isDurationGuard() {
        return this instanceof ABSDurationGuard;
    }

    public boolean isAndGuard() {
        return this instanceof ABSAndGuard;
    }

    public GuardView getLeftGuardView() {
        if (this instanceof ABSAndGuard andGuard) {
            return andGuard.getLeftGuard().getView();
        } else {
            return null;
        }
    }

    public GuardView getRightGuardView() {
        if (this instanceof ABSAndGuard andGuard) {
            return andGuard.getRightGuard().getView();
        } else {
            return null;
        }
    }

    public FutView getFutView() {
        if (this instanceof ABSFutureGuard futureGuard) {
            return futureGuard.fut.getView();
        } else {
            return null;
        }
    }

    public Aprational getMinTime() {
        if (this instanceof ABSDurationGuard durationGuard) {
            return durationGuard.getMinTime();
        } else {
            return null;
        }
    }

    public Aprational getMaxTime() {
        if (this instanceof ABSDurationGuard durationGuard) {
            return durationGuard.getMaxTime();
        } else {
            return null;
        }
    }

    public abstract String toABSString();

}
