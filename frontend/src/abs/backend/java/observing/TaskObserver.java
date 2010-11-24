package abs.backend.java.observing;

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
}
