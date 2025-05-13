/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.absunit;

import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;

import org.abs_models.backend.java.observing.TaskStackFrameView;
import org.abs_models.backend.java.observing.TaskView;

public class TestModel {

    private List<TaskView> ok = new LinkedList<>();
    private List<TaskView> failedAssertion = new LinkedList<>();
    private List<TaskView> error = new LinkedList<>();
    private List<TaskView> deadlocked = new LinkedList<>();

    private final HashMap<Integer, LinkedList<TestStatus>> activeTests = new HashMap<>();
    private final LinkedList<TestStatus> finishedTests = new LinkedList<>();
    private TestModelListener listener;

    public void testStarted(TaskView task) {

        final String testMethod;
        final String className;
        final List<Object> testMethodArguments;
        final LinkedList<Object> arguments = new LinkedList<>();
        if (task.getStack().hasFrames()) {
            final TaskStackFrameView currentF = task.getStack().getCurrentFrame();
            testMethod = currentF.getMethod().getName();
            className = currentF.getMethod().getClassView().getName();

            for (String parameterName : currentF.getVariableNames()) {
                arguments.add(currentF.getValue(parameterName));
            }

        } else {
            testMethod = task.getMethodName();
            className  = task.getTarget().getClassName();
            arguments.addAll(task.getArgs());
        }

        if (!task.getStack().hasFrames()) {
            System.out.println("Not yet started " + testMethod + " for task " + task.getID());
        } else {
            System.out.println("Test " + task.getStack().getCurrentFrame().getMethod() + " task: " + task.getID());
        }

        final TestStatus newTest = new TestStatus(task.getID(), testMethod, className,
                arguments,
                task.getStack().getFrames(), TestStatus.Status.ACTIVE);

        push(newTest);

        System.out.println("Test started: " + task.getMethodName() + " : " + testMethod);

        informListenersTestStarted(newTest);
    }

    private TestStatus peek(int taskID) {
        synchronized (activeTests) {
            LinkedList<TestStatus> testsOfTask = activeTests.get(taskID);
            if (testsOfTask == null || testsOfTask.size() == 0) {
                return null;
            }
            return testsOfTask.getLast();
        }
    }

    private TestStatus pop(int taskID) {
        synchronized (activeTests) {
            LinkedList<TestStatus> testsOfTask = activeTests.get(taskID);
            if (testsOfTask == null || testsOfTask.size() == 0) {
                return null;
            }
            System.out.println("Remove" + testsOfTask.getLast().getTaskID());
            return testsOfTask.removeLast();
        }
    }

    private void push(TestStatus ts) {
        synchronized (activeTests) {
            LinkedList<TestStatus> testsOfTask = activeTests.get(ts.getTaskID());
            if (testsOfTask == null) {
                testsOfTask = new LinkedList<>();
                activeTests.put(ts.getTaskID(), testsOfTask);
            }
            testsOfTask.addLast(ts);
        }
    }

    private void informListenersTestStarted(TestStatus newTest) {
        if (listener != null) {
            listener.testStarted(newTest);
        }
    }

    private void informListenersSystemFinished() {
        if (listener != null) {
            listener.systemFinished();
        }
    }

    public synchronized void testFinished(TaskView task) {
        System.out.println("Test " + task.getID() + " finished: " + task.hasException());

        final TestStatus status = pop(task.getID());
        if (task.hasException()) {
            if (task.getException().isAssertion()) {
                failedAssertion.add(task);
                status.setStatus(TestStatus.Status.FAILED);
            } else {
                error.add(task);
                status.setStatus(TestStatus.Status.ERROR);
            }
            status.setException(task.getException());
        } else if (task.isDeadlocked()) {
            deadlocked.add(task);
            status.setStatus(TestStatus.Status.DEADLOCKED);
        } else {
            ok.add(task);
            status.setStatus(TestStatus.Status.OK);
        }
        finishedTests.addLast(status);
        informListenersTestFinished(status);
    }

    private void informListenersTestFinished(TestStatus finishedTest) {
        if (listener != null) {
            listener.testFinished(finishedTest);
        }
    }

    public int sizeAll() {
        return active() + sizeFinished();
    }

    public int sizeFinished() {
        return finishedTests.size();
    }

    public int active() {
        int result = 0;
        synchronized (activeTests) {
            for (LinkedList<TestStatus> ts : activeTests.values()) {
                result += ts != null ? ts.size() : 0;
            }
        }
        return result;
    }

    public List<TestStatus> getAllFinishedTests() {
        return Collections.unmodifiableList(finishedTests);
    }

    public void addTestModelListener(TestModelListener modelListener) {
        this.listener = modelListener;
    }

    public int successful() {
        return ok.size();
    }

    public int failed() {
        return failedAssertion.size();
    }

    public int error() {
        return error.size();
    }

    public int deadlocked() {
        return deadlocked.size();
    }

    public void finished() {
        informListenersSystemFinished();
    }

    public void taskStep(TaskView task, String fileName, int line) {
        TestStatus status = peek(task.getID());
        int depth = task.getStack().getFrames() == null ? 0 : task.getStack().getFrames().size();
        if (status != null && status.depth() == depth) {
            status.updatePos(fileName, line);
        }
    }

}
