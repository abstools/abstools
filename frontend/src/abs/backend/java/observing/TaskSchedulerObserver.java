package abs.backend.java.observing;

public interface TaskSchedulerObserver {
    void taskCreated(TaskView task);
    void taskReady(TaskView view);
    void taskResumed(TaskView runningTask, GuardView view);
    void taskSuspended(TaskView task, GuardView guard);
}
