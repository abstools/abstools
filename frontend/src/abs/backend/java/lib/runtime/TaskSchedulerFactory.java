package abs.backend.java.lib.runtime;

public interface TaskSchedulerFactory {
    TaskScheduler createTaskScheduler(COG cog);
}
