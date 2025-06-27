/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.observing;

import org.apfloat.Aprational;

public interface GuardView {
    /**
     * Return true if the guard evaluates to true, false otherwise.
     */
    boolean isTrue();

    /**
     * Return true if the guard is an ExpressionGuard, false otherwise.
     */
    boolean isExpressionGuard();

    /**
     * Return true if the guard is a FutureGuard, false otherwise.
     */
    boolean isFutureGuard();

    /**
     * Return true if the guard is a DurationGuard, false otherwise.
     */
    boolean isDurationGuard();

    /**
     * Return true if the guard is an AndGuard, false otherwise.
     */
    boolean isAndGuard();

    /**
     * Return the left contained guard of an AndGuard.  Only call this method
     * if `isAndGuard` returns true.
     */
    GuardView getLeftGuardView();

    /**
     * Return the right contained guard of an AndGuard.  Only call this method
     * if `isAndGuard` returns true.
     */
    GuardView getRightGuardView();

    /**
     * Return the contained future of a FutureGuard.  Only call this method if
     * `isFutureGuard` returns true.
     */
    FutView getFutView();

    /**
     * Returns the minimum wakeup time of a DurationGuard.  Only call this
     * method if `isDurationGuard` returns true.
     */
    Aprational getMinTime();

    /**
     * Returns the maximum wakeup time of a DurationGuard.  Only call this
     * method if `isDurationGuard` returns true.
     */
    Aprational getMaxTime();

    /**
     * Return a human-readable representation of the guard, mainly for
     * debugging purposes.
     */
    String toABSString();
}
