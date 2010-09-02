package abs.backend.java.observing;

public interface TaskListener {
    void taskCreated(TaskView task);
    void taskSuspended(TaskView task, GuardObs guard);
    void taskStarted(TaskView task);
    void taskFinished(TaskView task);
}
