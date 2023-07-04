/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.lib.runtime;

/**
 * A guard that suspends until the value of its future becomes available.
 */
public class ABSFutureGuard extends ABSGuard {
    public final ABSFut<?> fut;
    private boolean isDone;

    public ABSFutureGuard(ABSFut<?> f) {
        this.fut = f;
        this.isDone = f.isDone();
    }

    public boolean await(COG cog) {
        if (!isDone) {
            fut.await(cog);
            isDone = true;
        }
        return true;
    }

    @Override
    public boolean isTrue() {
        return fut.isDone();
    }

    @Override
    public boolean staysTrue() {
        return true;
    }

    @Override
    public String toString() {
        return "Future Guard on " + fut;
    }

    @Override
    public String toABSString() {
        return "Fut " + fut.getID() + "?";
    }
}
