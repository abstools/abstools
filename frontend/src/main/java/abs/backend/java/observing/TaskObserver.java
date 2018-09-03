/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.observing;

import abs.backend.java.lib.types.ABSValue;

public interface TaskObserver {
    void taskStarted(TaskView task);

    void taskFinished(TaskView task);

    void taskBlockedOnFuture(TaskView task, FutView fut);

    void taskRunningAfterWaiting(TaskView view, FutView fut);


    /**
     * Is called when the given task does an execution step.
     * 
     * <b>Important:</b> This method is only called if debugging is turned on,
     * i.e., if the program has been started with <code>-Dabs.debug=true</code>
     * 
     * @param task
     *            the task that executes the step
     * @param fileName
     *            the absolute file name of the executed abs statement
     * @param line
     *            the line of the executed statement
     */
    void taskStep(TaskView task, String fileName, int line);

    /**
     * Called when a deadlock has been detected in a task. Note that if a task
     * deadlocked the taskFinished method will *not* be called!
     * 
     * @param task
     */
    void taskDeadlocked(TaskView task);
    
    /**
     * Called when a new stack frame is created in task.
     * This is called for every method that the task invokes synchronously, 
     * in particular it is also called for the initially called method.
     * 
     * @param task the task where the stack frame has been created
     * @param stackFrame the new stack frame
     */
    void stackFrameCreated(TaskView task, TaskStackFrameView stackFrame);

    /**
     * Called when a stack frame is popped.
     * @param oldFrame the removed stack frame
     */
    void stackFrameRemoved(TaskView task, TaskStackFrameView oldFrame);

    /**
     * Called when the value of a local variable has changed.
     * Note that this is also called for the initial value of a variable
     * @param name the name of the variable
     * @param v the new value
     */
    void localVariableChanged(TaskStackFrameView stackFrame, String name, ABSValue v);

}
