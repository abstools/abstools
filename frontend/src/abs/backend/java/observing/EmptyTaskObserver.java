package abs.backend.java.observing;

public class EmptyTaskObserver implements TaskObserver {

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
}
