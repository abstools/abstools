/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.absunit;

import java.util.List;

import abs.backend.java.lib.runtime.ABSException;
import abs.backend.java.lib.types.ABSValue;
import abs.backend.java.observing.TaskStackFrameView;

/**
 * An instance of this class is created when a test method is invoked. The
 * created instance is used to track the progress and status of the test.
 * Eentually, it can be queried if the test passed or failed. There are
 * different failure reasons like FAILED (assertion violated), deadlocked or an
 * (internal) error.
 * 
 * The status instance also tracks the currently executed line of code which can
 * be used to identify the point of failure.
 * 
 * 
 * @author Richard Bubel
 * 
 */
public class TestStatus {

    /* enumeration of possible test statuses */
    enum Status {
        /** test method running or at least task created */
        ACTIVE,
        /* test passed */
        OK,
        /* test assertion failed */
        FAILED,
        /* test deadlocked */
        DEADLOCKED,
        /* test error */
        ERROR
    }

    /* the status of the test */
    private Status status;

    /* the id of the task executing the tracked test */
    private final int taskID;

    /* the name of the test method and its class */
    private final String testMethod;
    private final String className;

    /** The value of the parameters of the test method at invocation time */
    private final List<ABSValue> args;

    /*
     * the last executed line of code and the name of the file containing the
     * test method
     */
    private int line = -1;
    private String fileName = "";

    /* not iff a test assertion failed or an internal error occurred */
    private ABSException exception;

    /*
     * depth of stack frame of the test's task when the test method has been
     * invoked
     */
    private final int framesDepth;

    public TestStatus(int id, String method, String className, List<ABSValue> args,
            List<? extends TaskStackFrameView> frames, Status status) {
        taskID = id;
        testMethod = method;
        this.className = className;
        this.args = args;
        this.framesDepth = frames == null ? 0 : frames.size();
        this.status = status;
    }

    /* the tasks' stack frame depth when starting the thread */
    public int depth() {
        return framesDepth;
    }

    /* formatted text which can be displayed */
    public String displayString() {
        String result = "<html><body>";
        result += "<strong>Class:</strong> " + className + "<br>";
        result += "<strong>Method:</strong> " + testMethod + "<br>";
        result += "<strong>Arguments:</strong> " + args + "<br>";
        result += "<strong>Status:</strong> " + status + "<br>";
        if (getException() != null) {
            if (exception.isAssertion()) {
                result += "<strong>Reason:</strong> Assertion failed.";
                if (line > 0) {
                    result += " at line " + line + " in file " + fileName + "\n";
                }
            } else if (!exception.isDeadlock()) {
                result += "<strong>Reason:</strong> Internal Error,\n";
            }
        }
        return result + "</body></html>";
    }

    /**
     * ABS exception instance of Java backend used to indicate a failure
     * situation
     */
    public ABSException getException() {
        return exception;
    }

    /* the name of the testmethod */
    public String getMethodName() {
        return testMethod;
    }

    /* returns the test status */
    public Status getStatus() {
        return status;
    }

    /* returns the id of the task running the test method */
    public int getTaskID() {
        return taskID;
    }

    /*
     * set the ABSException used by the Java backend to represent a failure
     * situation
     */
    public void setException(ABSException exception) {
        this.exception = exception;
    }

    /* sets the test status */
    public void setStatus(Status status) {
        this.status = status;
    }

    /* toString */
    public String toString() {
        String result = "";
        result += testMethod;
        if (getException() != null && getException().isAssertion()) {
            result += " at: " + getException().getMessage().substring(0, getException().getMessage().lastIndexOf(':'));
        }
        return result;
    }

    /**
     * called to update the current execution position of the test method the
     * information is used to identify the source code position of the failed
     * assertion
     */
    public void updatePos(String fileName, int line) {
        this.fileName = fileName;
        this.line = line;
    }
}
