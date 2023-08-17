/** 
 * Copyright (c), The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.lib.runtime;

import org.abs_models.backend.java.lib.types.ABSRational;
import org.apfloat.Aprational;
import java.util.logging.Logger;

/**
 * A guard that suspends until the logical clock has advanced by at least the
 * specified amount.
 */
public class ABSDurationGuard extends ABSGuard {

    protected static final Logger log = Logging.getLogger(ABSDurationGuard.class.getName());

    /**
     * The minimum absolute time when the guard becomes enabled.
     */
    Aprational min_time;
    /**
     * The absolute time to set the clock to if no one else is waiting; should
     * always be greater or equal to min_time.
     */
    Aprational max_time;

    /**
     * Construct an ABSDurationGuard.  Note that the constructor arguments are
     * *relative* times that are converted to absolute times internally.
     *
     * @param minduration The minimum duration to suspend the process; if the
     *   clock advances by more than this then the process becomes
     *   schedulable.
     * @param maxduration The maximum duration to suspend the process; if no
     *   other process waits, the clock will advance by this amount.
     */
    public ABSDurationGuard(ABSRational minduration, ABSRational maxduration) {
        Aprational current_time = ABSRuntime.getRuntime().getClock();
        this.min_time = current_time.add(minduration.toAprational());
        this.max_time = current_time.add(maxduration.toAprational());
    }

    /**
     * Return the minimum absolute time when the guard becomes true.
     */
    public Aprational getMinTime() {
        return min_time;
    }

    /**
     * Return the minimum absolute time when the guard becomes true.
     */
    public Aprational getMaxTime() {
        return max_time;
    }

    @Override
    public boolean isTrue() {
        return ABSRuntime.getRuntime().getClock().compareTo(min_time) >= 0;
    }

    @Override
    public boolean staysTrue() {
        return true;
    }

    /**
     * {@inheritDoc}
     * <p>
     * If the guard evaluates to false, this method calls {@link
     * ABSRuntime#addDurationGuard} and waits.  The runtime will signal once
     * the clock advances to this guard's minimum wakeup time.
     */
    @Override
    public synchronized boolean await(COG cog, Task<?> task) {
        log.finest(() -> "Awaiting until time between " + getMinTime() + " and " + getMaxTime());

        boolean mustSuspend = !isTrue();

        if (mustSuspend) {
            // Update ABSRuntime state *before* notifying our own cog, so in
            // case everyone's idle the runtime will already know it needs to
            // wake us.
            ABSRuntime.getRuntime().addDurationGuard(this);
            cog.notifyAwait(task);
        }
        while (!isTrue()) {
            try {
                this.wait();
            } catch (InterruptedException e) {
                // This code copied over from ABSFut#await
                log.finest(() -> this + " was interruped during await");
                Thread.currentThread().interrupt();
                break;
            }
        }
        if (mustSuspend) {
            cog.notifyWakeup(task);
        }
        // we only reach this point once we became true
        return true;
    }

    @Override
    public String toABSString() {
        return "wakeup at " + getMinTime() + " to " + getMaxTime();
    }
}
