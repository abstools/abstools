package abs.backend.java.scheduling;

import java.util.ArrayList;
import java.util.List;

import abs.backend.java.lib.runtime.ABSGuard;
import abs.backend.java.observing.TaskObserver;
import abs.backend.java.observing.TaskSchedulerView;
import abs.backend.java.observing.TaskView;

public abstract class AbstractTaskSchedulerView implements TaskSchedulerView {

    private List<TaskObserver> observers;

    @Override
    public synchronized void registerTaskObserver(TaskObserver listener) {
        getObservers().add(listener);
    }

    protected synchronized List<TaskObserver> getObservers() {
        if (observers == null) {
            observers = new ArrayList<TaskObserver>(1);
        }
        return observers;
    }

    public void taskReady(TaskView view) {
        for (TaskObserver l : getObservers()) {
            l.taskReady(view);
        }
    }

    protected synchronized void taskDeadlocked(TaskView task) {
        for (TaskObserver l : getObservers()) {
            l.taskDeadlocked(task);
        }

    }

    synchronized void taskResumed(TaskView runningTask, ABSGuard g) {
        for (TaskObserver l : getObservers()) {
            l.taskResumed(runningTask, g.getView());
        }
    }

    public synchronized void taskSuspended(TaskView runningTask, ABSGuard g) {
        for (TaskObserver l : getObservers()) {
            l.taskSuspended(runningTask, g.getView());
        }
    }

    public void taskStarted(TaskView view) {
        for (TaskObserver l : getObservers()) {
            l.taskStarted(view);
        }
    }

    public synchronized void taskFinished(TaskView view) {
        for (TaskObserver l : getObservers()) {
            l.taskFinished(view);
        }
    }

    public synchronized void taskAdded(TaskView view) {
        for (TaskObserver l : getObservers()) {
            l.taskCreated(view);
        }
    }

}
