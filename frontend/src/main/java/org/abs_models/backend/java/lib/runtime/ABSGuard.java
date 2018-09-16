/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.lib.runtime;

import org.abs_models.backend.java.observing.FutView;
import org.abs_models.backend.java.observing.GuardView;

public abstract class ABSGuard {
    public abstract boolean isTrue();

    /**
     * 
     * @return whether this guard could become false again in the future
     */
    public boolean await() {
        return !staysTrue();
    }

    private GuardView view;

    public GuardView getView() {
        if (view == null)
            view = new View();
        return view;
    }

    public boolean staysTrue() {
        return true;
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

        @Override
        public String toABSString() {
            return ABSGuard.this.toABSString();
        }

    }

    public abstract String toABSString();

}
