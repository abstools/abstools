package abs.backend.java.lib.runtime;

import java.util.List;

import abs.backend.java.lib.runtime.SimpleTaskScheduler.TaskInfo;

public interface SchedulingStrategy {
    TaskInfo schedule(TaskScheduler scheduler, List<TaskInfo> scheduableTasks);
}
