package abs.backend.java.observing;

import java.util.List;

import abs.backend.java.lib.types.ABSValue;

public class EmptyCompleteObserver implements CompleteObserver {

    @Override
    public void systemStarted() {
    }

    @Override
    public void newCOGCreated(COGView cog, ObjectView initialObject) {
    }

    @Override
    public void objectCreated(ObjectView o) {
    }

    @Override
    public void objectInitialized(ObjectView o) {
    }

    @Override
    public void methodCalled(String method, List<ABSValue> args) {
    }

    @Override
    public void fieldRead(String field, ABSValue value) {
    }

    @Override
    public void fieldUpdated(String field, ABSValue oldValue, ABSValue newValue) {
    }

    @Override
    public void taskCreated(TaskView task) {
    }

    @Override
    public void taskReady(TaskView view) {
    }

    @Override
    public void taskResumed(TaskView runningTask, GuardView view) {
    }

    @Override
    public void taskSuspended(TaskView task, GuardView guard) {
    }

    @Override
    public void systemFinished() {
    }
}
