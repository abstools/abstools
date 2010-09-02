package abs.backend.java.observing;

public class EmptyTaskListener implements TaskListener {

    @Override
    public void taskCreated(TaskView task) {
    }

    @Override
    public void taskSuspended(TaskView task, GuardObs guard) {
    }

    @Override
    public void taskStarted(TaskView task) {
    }

    @Override
    public void taskFinished(TaskView task) {
    }

}
