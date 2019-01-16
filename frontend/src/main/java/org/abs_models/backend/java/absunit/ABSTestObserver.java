/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.absunit;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.abs_models.backend.java.lib.runtime.ABSException;
import org.abs_models.backend.java.lib.types.ABSValue;
import org.abs_models.backend.java.observing.COGView;
import org.abs_models.backend.java.observing.FutObserver;
import org.abs_models.backend.java.observing.FutView;
import org.abs_models.backend.java.observing.ObjectView;
import org.abs_models.backend.java.observing.RegistratingObserver;
import org.abs_models.backend.java.observing.SchedulerObserver;
import org.abs_models.backend.java.observing.TaskObserver;
import org.abs_models.backend.java.observing.TaskStackFrameView;
import org.abs_models.backend.java.observing.TaskView;

public class ABSTestObserver extends RegistratingObserver implements FutObserver, TaskObserver, SchedulerObserver {

    private final TestModel model;
    private final ABSUnitGUI gui;

    private Map<Integer, LinkedList<SyncTestInfo>> syncTests = new HashMap<>();

    class SyncTestInfo {
        public TaskView task;
        public final int frameDepth;

        SyncTestInfo(TaskView task, int frameDepth) {
            this.task = task;
            this.frameDepth = frameDepth;
        }
    }

    public ABSTestObserver() {
        model = new TestModel();
        gui = new ABSUnitGUI(model);
        gui.pack();
        gui.setVisible(true);
    }

    public void systemStarted() {
        super.systemStarted();
    }

    @Override
    public void systemFinished() {
        super.systemFinished();
        model.finished();
    }

    @Override
    public void systemError(ABSException e) {
        super.systemError(e);
    }

    public void newCOGCreated(COGView cog, ObjectView o) {
        super.newCOGCreated(cog, o);
    }

    @Override
    public void objectCreated(ObjectView o) {
        super.objectCreated(o);
    }

    @Override
    public void methodCalled(ObjectView object, String method, List<ABSValue> args) {
    }

    String objectString(ObjectView o) {
        return o.getClassName() + "[" + o.getID() + "]";
    }

    @Override
    public void taskCreated(TaskView task) {
        task.registerTaskListener(this);
        task.getFuture().registerFutObserver(this);
    }

    @Override
    public void taskStarted(TaskView task) { }

    @Override
    public void taskFinished(TaskView task) {
        synchronized (syncTests) {
            if (runsSyncTest(task)) {
                if (isUnitTest(task.getMethodName())) {
//                    System.out.println("Name: "+task.getMethodName() + " " + task.getID());
                    model.testFinished(task);
                }
            }
        }
    }

    private boolean isUnitTest(String methodName) {
//        return Class.forName(className).getMethod(methodName, parameterTypes).getAnnotations();
        return methodName.startsWith("test");
    }

    @Override
    public void taskBlockedOnFuture(TaskView task, FutView fut) {}

    @Override
    public void taskRunningAfterWaiting(TaskView view, FutView fut) {}

    @Override
    public void taskStep(TaskView task, String fileName, int line) {
        model.taskStep(task, fileName, line);
    }

    @Override
    public void taskDeadlocked(TaskView task) {
//        System.out.println("Task deadlocked " + task.hasException() + ":" + task.getID());
        if (isUnitTest(task.getMethodName())) {
            model.testFinished(task);
            pop(task.getID());
        }
    }

    @Override
    public void stackFrameCreated(TaskView task, TaskStackFrameView stackFrame) {
        if (isUnitTest(stackFrame.getMethod().getName())) {
            pushSyncTestInfo(task, new SyncTestInfo(task, stackFrame.getStack().getFrames().size()));
            model.testStarted(task);
        }
    }

    private SyncTestInfo peek(int taskID) {
        synchronized (syncTests) {
            LinkedList<SyncTestInfo> testsOfTask = syncTests.get(taskID);
            if (testsOfTask == null || testsOfTask.size() == 0) {
                return null;
            }
            return testsOfTask.getLast();
        }
    }

    private SyncTestInfo pop(int taskID) {
        synchronized (syncTests) {
            LinkedList<SyncTestInfo> testsOfTask = syncTests.get(taskID);
            if (testsOfTask == null || testsOfTask.size() == 0) {
                return null;
            }
            return testsOfTask.removeLast();
        }
    }

    private void pushSyncTestInfo(TaskView task, SyncTestInfo syncTestInfo) {
        synchronized (syncTests) {
            LinkedList<SyncTestInfo> testsOfTask = syncTests.get(task.getID());
            if (testsOfTask == null) {
                testsOfTask = new LinkedList<>();
                syncTests.put(task.getID(), testsOfTask);
            }
            testsOfTask.addLast(syncTestInfo);
        }
    }

    private boolean runsSyncTest(TaskView task) {
        return peek(task.getID()) != null;
    }

    @Override
    public void localVariableChanged(TaskStackFrameView stackFrame, String name, ABSValue v) {
    }

    @Override
    public void onResolved(FutView fut, ABSValue value) {
    }

    @Override
    public void stackFrameRemoved(TaskView task, TaskStackFrameView oldFrame) {
        synchronized (syncTests) {
            if (isUnitTest(oldFrame.getMethod().getName())) {
                model.testFinished(task);
                pop(task.getID());
            }
        }
    }
}
