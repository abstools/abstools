/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.observing;

public interface COGView {
    TaskSchedulerView getSchedulerView();

    void registerObjectCreationListener(ObjectCreationObserver listener);

    void registerObjectCreationListener(String className, ObjectCreationObserver e);

    int getID();
}
