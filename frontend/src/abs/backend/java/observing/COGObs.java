package abs.backend.java.observing;

import java.util.List;

public interface COGObs {
    List<TaskObs> getNewTasks();
    List<TaskObs> getSuspendedTasks();
    TaskObs getActiveTask();
}
