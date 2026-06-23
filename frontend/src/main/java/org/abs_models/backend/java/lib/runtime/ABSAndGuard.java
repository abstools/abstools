/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.lib.runtime;

/**
 * A guard that composes two guards.  It is enabled whenever both its
 * constituent guards are enabled.
 */
public class ABSAndGuard extends ABSGuard {
    public final ABSGuard left;
    public final ABSGuard right;

    public ABSAndGuard(COG cog, ABSGuard l, ABSGuard r) {
        assert l.getCog() == cog && r.getCog() == cog : "Trying to assemble an AndGuard with guards on different cogs";
        super(cog);
        left = l;
        right = r;
    }

    @Override
    public boolean staysTrue() {
        return left.staysTrue() && right.staysTrue();
    }

    @Override
    public boolean isTrue() {
        return left.isTrue() && right.isTrue();
    }

    public String toString() {
        return "ANDGuard " + left + " AND " + right;
    }

    public ABSGuard getLeftGuard() {
        return left;
    }

    public ABSGuard getRightGuard() {
        return right;
    }

    public boolean await() {
        // We don't ourselves notify the cog about idleness; the nested guards
        // will take care of it sequentially.  (I.e., we only call right.await
        // after left.await is done.)

        // FIXME: this is wrong in general; if both guards are of type
        // DurationGuard, we need to unify their intervals, *not* execute them
        // sequentially.
        boolean b = left.await();
        boolean b2 = right.await();
        return b && b2;

    }

    @Override
    public String toABSString() {
        return left.toABSString() + " & " + right.toABSString();
    }
}
