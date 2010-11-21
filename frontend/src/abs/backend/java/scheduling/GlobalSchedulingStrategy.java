package abs.backend.java.scheduling;

public interface GlobalSchedulingStrategy {
    ScheduleAction choose(ScheduleOptions options);
}
