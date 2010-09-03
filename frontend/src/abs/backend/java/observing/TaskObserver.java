package abs.backend.java.observing;

public interface TaskObserver {
    void taskCreated(TaskView task);
    void taskSuspended(TaskView task, GuardView guard);
    void taskStarted(TaskView task);
    void taskFinished(TaskView task);
    void taskBlockedOnFuture(TaskView task, FutView fut);
}
