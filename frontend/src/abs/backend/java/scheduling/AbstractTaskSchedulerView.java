package abs.backend.java.scheduling;

import java.util.ArrayList;
import java.util.List;

import abs.backend.java.lib.runtime.ABSGuard;
import abs.backend.java.observing.TaskSchedulerObserver;
import abs.backend.java.observing.TaskObserver;
import abs.backend.java.observing.TaskSchedulerView;
import abs.backend.java.observing.TaskView;

public abstract class AbstractTaskSchedulerView implements TaskSchedulerView {

    private List<TaskSchedulerObserver> observers;

    @Override
    public synchronized void registerTaskSchedulerObserver(TaskSchedulerObserver listener) {
        getObservers().add(listener);
    }

    protected synchronized List<TaskSchedulerObserver> getObservers() {
        if (observers == null) {
            observers = new ArrayList<TaskSchedulerObserver>(1);
        }
        return observers;
    }

    synchronized void taskResumed(TaskView runningTask, ABSGuard g) {
        for (TaskSchedulerObserver l : getObservers()) {
            l.taskResumed(runningTask, g.getView());
        }
    }

    public synchronized void taskSuspended(TaskView runningTask, ABSGuard g) {
        for (TaskSchedulerObserver l : getObservers()) {
            l.taskSuspended(runningTask, g.getView());
        }
    }

    public synchronized void taskAdded(TaskView view) {
        for (TaskSchedulerObserver l : getObservers()) {
            l.taskCreated(view);
        }
    }

    public synchronized void taskReady(TaskView view) {
        for (TaskSchedulerObserver l : getObservers()) {
            l.taskReady(view);
        }
    }
    
}
