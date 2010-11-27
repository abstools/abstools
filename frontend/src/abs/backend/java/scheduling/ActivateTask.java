package abs.backend.java.scheduling;

import abs.backend.java.lib.runtime.COG;
import abs.backend.java.lib.runtime.Task;

public class ActivateTask extends ScheduleAction {

    private final Task<?> task;

    public ActivateTask(COG cog, Task<?> task) {
        super(cog);
        if (task == null) {
            throw new IllegalArgumentException("Task is null");
        }
        this.task = task;
         
    }
    
    @Override
    public String toString() {
        return "Activate task "+task.getID();
    }    

    @Override
    public String shortString() {
        return getCOG().getID() + ",A," + task.getID();
    }
    
    @Override
    public Task<?> getTask() {
        return task;
    }

}
