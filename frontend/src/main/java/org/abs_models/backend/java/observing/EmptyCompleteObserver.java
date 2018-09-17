/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.observing;

import java.util.List;

import org.abs_models.backend.java.lib.runtime.ABSException;
import org.abs_models.backend.java.lib.types.ABSValue;

public class EmptyCompleteObserver implements CompleteObserver {

    @Override
    public void systemStarted() {
    }

    @Override
    public void newCOGCreated(COGView cog, ObjectView initialObject) {
    }

    @Override
    public void objectCreated(ObjectView o) {
    }

    @Override
    public void objectInitialized(ObjectView o) {
    }

    @Override
    public void methodCalled(ObjectView o, String method, List<ABSValue> args) {
    }

    @Override
    public void fieldRead(ObjectView o, String field, ABSValue value) {
    }

    @Override
    public void fieldUpdated(ObjectView o, String field, ABSValue oldValue, ABSValue newValue) {
    }

    @Override
    public void taskCreated(TaskView task) {
    }

    @Override
    public void taskReady(TaskView view) {
    }

    @Override
    public void taskResumed(TaskView runningTask, GuardView view) {
    }

    @Override
    public void taskSuspended(TaskView task, GuardView guard) {
    }

    @Override
    public void systemFinished() {
    }

    @Override
    public void systemError(ABSException e) {
    }
}
