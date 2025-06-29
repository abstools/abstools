/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.observing;

public interface FutView {
    TaskView getResolvingTaskView();

    boolean isResolved();

    Object getValue();

    void registerFutObserver(FutObserver obs);

    int getID();
}
