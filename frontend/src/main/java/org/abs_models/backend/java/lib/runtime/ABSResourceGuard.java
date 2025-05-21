/** 
 * Copyright (c), The ABS Project. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.lib.runtime;

import java.util.logging.Logger;

import org.apfloat.Aprational;

/**
 * A "guard" that implements the Cost: annotation from Timed ABS.
 *
 * We implement resource consumption as a guard, since {@code [Cost: x] skip;}
 * is, from a cog's point of view, equivalent to {@code f.get;}: both block
 * the whole cog until some external condition is fulfilled ({@code x}
 * resources consumed or {@code f} filled, respectively).
 */
public class ABSResourceGuard extends ABSGuard {

    protected static final Logger log = Logging.getLogger(ABSResourceGuard.class.getName());

    /**
     * The amount of resources needed.
     */
    Aprational resources_needed;
    /**
     * The amount of resources consumed.
     */
    Aprational resources_consumed;

    /**
     * Construct an ABSResourceGuard.
     *
     * @param resources The amount of resources needed.
     */
    public ABSResourceGuard(Aprational resources) {
        this.resources_needed = resources;
        this.resources_consumed = Aprational.ZERO;
        if (resources_needed.signum() < 0) {
            log.severe(() -> this + " trying to consume a negative amount of resources(" + resources_needed + "), continuing with 0 required resources instead.");
            this.resources_needed = Aprational.ZERO;
        }
    }

    /**
     * Return the remaining amount of resources needed.
     */
    public Aprational getResourcesNeeded() {
        return resources_needed.subtract(resources_consumed);
    }

    /**
     * Consume some resources.
     *
     * @param amount The amount of resources consumed; should be not greater
     *        than the remaining needed amount.
     * @return true if enough resources have been consumed, false otherwise.
     */
    public boolean consumeResources(Aprational amount) {
        resources_consumed = resources_consumed.add(amount);
        log.finest(() -> "Got " + resources_consumed + " of wanted " + resources_needed + " resources");
        return isTrue();
    }

    @Override
    public boolean isTrue() {
        // return true also if we consumed more resources than needed
        return resources_needed.compareTo(resources_consumed) <= 0;
    }

    @Override
    public boolean staysTrue() {
        return true;
    }

    /**
     * {@inheritDoc}
     * <p>
     
     * The Duration "guard" is only ever used for blocking the whole cog.  If
     * the guard evaluates to false, this method calls {@link
     * ABSRuntime#addResourceGuard} and waits.  The runtime will signal once
     * the guard has been given the required amount of resources, then this
     * guard will unblock the cog.
     */
    @Override
    public synchronized boolean await(COG cog, Task<?> task) {
        log.finest(() -> "Consuming " + resources_needed + " resources ");

        boolean mustSuspend = !isTrue();

        if (mustSuspend) {
            log.finest(() -> "Waiting for resources from " + cog.getDC());
            // Update ABSRuntime state *before* notifying our own cog to avoid
            // a small race conditon where the runtime might decide everyone's
            // idle.
            ABSRuntime.getRuntime().addResourceGuard(this, cog.getDC());
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
        return "consuming " + resources_consumed + " of " + resources_needed + " resources";
    }
}
