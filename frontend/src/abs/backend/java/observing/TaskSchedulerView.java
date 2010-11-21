package abs.backend.java.observing;

import java.util.List;

public interface TaskSchedulerView {
    List<TaskView> getNewTasks();

    List<TaskView> getSuspendedTasks();

    TaskView getActiveTask();

    void registerTaskObserver(TaskObserver listener);
}
