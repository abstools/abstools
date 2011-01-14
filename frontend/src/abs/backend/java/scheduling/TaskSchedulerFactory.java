package abs.backend.java.scheduling;

import abs.backend.java.lib.runtime.ABSRuntime;
import abs.backend.java.lib.runtime.ABSThreadManager;
import abs.backend.java.lib.runtime.COG;

public interface TaskSchedulerFactory {
    TaskScheduler createTaskScheduler(ABSRuntime absRuntime, COG cog, ABSThreadManager m);
}
