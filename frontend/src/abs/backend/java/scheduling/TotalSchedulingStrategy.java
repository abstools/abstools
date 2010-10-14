package abs.backend.java.scheduling;

/**
 * A scheduling strategy which does global and task scheduling.
 * Can be set by the property -Dabs.totalscheduler=<classname>
 * 
 * @author Jan Schäfer
 *
 */
public interface TotalSchedulingStrategy extends 
        GlobalSchedulingStrategy,
        TaskSchedulingStrategy {

}
