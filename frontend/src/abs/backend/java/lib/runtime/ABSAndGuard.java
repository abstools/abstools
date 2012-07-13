/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.lib.runtime;

public class ABSAndGuard extends ABSGuard {
    public final ABSGuard left;
    public final ABSGuard right;

    public ABSAndGuard(ABSGuard l, ABSGuard r) {
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
        boolean b = left.await();
        boolean b2 = right.await();
        return b || b2;

    }

    @Override
    public String toABSString() {
        return left.toABSString() + " & " + right.toABSString();
    }
}
