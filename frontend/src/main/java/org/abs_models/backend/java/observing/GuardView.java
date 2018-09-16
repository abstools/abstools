/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.observing;

public interface GuardView {
    boolean isTrue();

    boolean isExpressionGuard();

    boolean isFutureGuard();

    boolean isAndGuard();

    GuardView getLeftGuard();

    GuardView getRightGuard();

    FutView getFuture();

    String toABSString();
}
