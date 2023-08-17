/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.lib.runtime;

import org.abs_models.backend.java.lib.types.ABSBool;
import java.util.logging.Logger;

/**
 * A guard that suspends while the specified condition evaluates to false.
 * <p>
 * This is the only non-monotonous guard; care must be taken to call the
 * proper method ({@link #await} or {@link #isTrue}) when querying the guard's
 * status; see the documentation of these methods for details.
 */
public abstract class ABSExpGuard extends ABSGuard {

    protected static final Logger log = Logging.getLogger(ABSExpGuard.class.getName());
    /**
     * This flag keeps track on whether we have to tell the cog that we
     * changed status.  We want to notify the cog that we're runnable if we
     * were not runnable during the previous scheduling check, but not every
     * time we're checking.  Similarly, we want to notify the cog we're now
     * sleeping only if we were previously runnable.
     */
    private boolean wasPreviouslyTrue = true;

    public abstract ABSBool evaluateExp();

    @Override
    public boolean staysTrue() {
        return false;
    }

    /**
     * {@inheritDoc}
     * <p>
     * DO NOT call this method to make scheduling decisions; call {@link
     * #await} instead in that case, since we need to inform the cog every
     * time our status changes so time advance can proceed properly.
     */
    @Override
    public boolean isTrue() {
        return evaluateExp().toBoolean();
    }

    /**
     * {@inheritDoc}
     *
     * ONLY call this method to make scheduling decisions, since it
     * communicates readiness changes to the runtime.  Call {@link #isTrue}
     * instead to print or otherwise display the guard's status.
     */
    public boolean await(COG cog, Task<?> task) {
        boolean isTrue = isTrue();
        if (!isTrue && wasPreviouslyTrue) {
            log.finest(() -> "Condition of " + this + " changed from true to false; telling cog we can't run...");
            cog.notifyAwait(task);
        } else if (isTrue && !wasPreviouslyTrue) {
            log.finest(() -> "Condition of " + this + " changed from false to true; telling cog we can run...");
            cog.notifyWakeup(task);
        }
        wasPreviouslyTrue = isTrue;
        return isTrue;
    }

    @Override
    public String toString() {
        return "ExpressionGuard (" + isTrue() + ")";
    }

    @Override
    public String toABSString() {
        return "<exp>";
    }
}
