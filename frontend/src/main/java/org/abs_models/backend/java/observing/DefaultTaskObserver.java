/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.observing;

import org.abs_models.backend.java.lib.types.ABSValue;

/**
 * A default implementation of the {@link TaskObserver} interface
 * 
 * @author Jan Schäfer
 */
public class DefaultTaskObserver implements TaskObserver {

    @Override
    public void taskStarted(TaskView task) {
    }

    @Override
    public void taskFinished(TaskView task) {
    }

    @Override
    public void taskBlockedOnFuture(TaskView task, FutView fut) {
    }

    @Override
    public void taskRunningAfterWaiting(TaskView view, FutView fut) {
    }

    @Override
    public void taskStep(TaskView task, String fileName, int line) {
    }

    @Override
    public void taskDeadlocked(TaskView task) {
    }

    @Override
    public void stackFrameCreated(TaskView task, TaskStackFrameView stackFrame) {
    }

    @Override
    public void localVariableChanged(TaskStackFrameView stackFrame, String name, ABSValue v) {
        
    }

    @Override
    public void stackFrameRemoved(TaskView task, TaskStackFrameView oldFrame) {        
    }
}
