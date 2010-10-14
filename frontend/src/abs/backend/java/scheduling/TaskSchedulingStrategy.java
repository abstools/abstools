package abs.backend.java.scheduling;

import java.util.List;

import abs.backend.java.scheduling.SimpleTaskScheduler.TaskInfo;

public interface TaskSchedulingStrategy {
    TaskInfo schedule(TaskScheduler scheduler, List<TaskInfo> scheduableTasks);
}
