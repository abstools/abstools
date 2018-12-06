/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.observing;

import java.util.List;

import org.abs_models.backend.java.lib.runtime.ABSException;
import org.abs_models.backend.java.lib.types.ABSValue;

public interface TaskView {
    /**
     * The Task that did the asynchronous call leading to this task is null for
     * the main task, otherwise is never null
     * 
     * @return the calling task
     */
    TaskView getSender();

    /**
     * The source object of the asynchronous call leading to this task is null
     * for the main task, otherwise is never null
     * 
     * @return the source object, or <code>null</code> if there is no source object
     */
    ObjectView getSource();

    ObjectView getTarget();

    COGView getCOG();

    String getMethodName();

    List<ABSValue> getArgs();

    FutView getFuture();

    void registerTaskListener(TaskObserver listener);

    int getID();

    boolean isDeadlocked();

    boolean hasException();

    ABSException getException();
    
    TaskStackView getStack();
}
