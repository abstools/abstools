/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.lib.runtime;

/**
 * A guard that never suspends.
 */
public class ABSTrueGuard extends ABSGuard {

    @Override
    public boolean isTrue() {
        return true;
    }

    @Override
    public boolean staysTrue() {
        return true;
    }

    @Override
    public boolean await(COG cog, Task<?> task) {
        return true;
    }

    @Override
    public String toString() {
        return "TrueGuard";
    }

    @Override
    public String toABSString() {
        return "True";
    }
}
