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
 */
public abstract class ABSGuard {

    /**
     * Return whether the guard is true; i.e., whether the process waiting on
     * the guard can be scheduled.
     * <p>
     * NOTE: It is important to know whether to call this method or the {@link
     * #await} method.  The await method informs the cog about the task's
     * status, so should be called when the result will lead to a scheduling
     * decision.  The {@code isTrue} method should be called when we need to
     * display the guard's status in some way, e.g. for pretty-printing or
     * debugging purposes.
     * <p>
     * To obey this protocol is most important for guards of type {@link
     * ABSExpGuard}, since that guard changes its value frequently, and the
     * runtime must be informed about its effective status at all times.
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
     * <p>
     * This method is allowed to suspend the thread, but should arrange to be
     * woken up, e.g., by the future becoming available.  This method will be
     * called multiple times, hence should not do too much work once the guard
     * is true.
     * <p>
     * All implementations of this method must inform the cog about their
     * status via the methods {@link COG#notifyAwait} and {@link
     * COG#notifyWakeup}.
     *
     * @param cog the cog of the task that might be suspended.
     * @param task the task to be suspended.
     * @return the status of the guard (true if runnable, false if not).
     */
    public abstract boolean await(COG cog, Task<?> task);

    private GuardView view;

    public GuardView getView() {
        if (view == null)
            view = new View();
        return view;
    }

    private class View implements GuardView {

        public boolean isTrue() {
            return ABSGuard.this.isTrue();
        }

        public boolean isExpressionGuard() {
            return ABSGuard.this instanceof ABSExpGuard;
        }

        public boolean isFutureGuard() {
            return ABSGuard.this instanceof ABSFutureGuard;
        }

        public boolean isDurationGuard() {
            return ABSGuard.this instanceof ABSDurationGuard;
        }

        public boolean isAndGuard() {
            return ABSGuard.this instanceof ABSAndGuard;
        }

        public GuardView getLeftGuard() {
            return ((ABSAndGuard) ABSGuard.this).getLeftGuard().getView();
        }

        public GuardView getRightGuard() {
            return ((ABSAndGuard) ABSGuard.this).getRightGuard().getView();
        }

        public FutView getFuture() {
            return ((ABSFutureGuard) ABSGuard.this).fut.getView();
        }

        public Aprational getMinTime() {
            return ((ABSDurationGuard) ABSGuard.this).getMinTime();
        }

        public Aprational getMaxTime() {
            return ((ABSDurationGuard) ABSGuard.this).getMaxTime();
        }

        @Override
        public String toABSString() {
            return ABSGuard.this.toABSString();
        }

    }

    public abstract String toABSString();

}
