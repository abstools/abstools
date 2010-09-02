package abs.backend.java.observing;

import java.util.List;

public interface SchedulerView {
    List<TaskView> getNewTasks();
    List<TaskView> getSuspendedTasks();
    TaskView getActiveTask();
    
    void registerTaskActionListener(TaskListener listener);
}
