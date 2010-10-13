package abs.backend.java.scheduling;

import abs.backend.java.lib.runtime.COG;

public interface TaskSchedulerFactory {
    TaskScheduler createTaskScheduler(COG cog);
}
