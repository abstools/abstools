package abs.backend.java.scheduling;

import abs.backend.java.lib.runtime.Task;

public class StepTask extends ScheduleAction {
    private final Task<?> task;

    public StepTask(Task<?> task) {
        super(task.getCOG());
        this.task = task;
    }
    
    public Task<?> getTask() {
        return task;
    }
    
    public String toString() {
        return "Step task "+task;
    }
    
    @Override
    public String shortString() {
        return "T"+task.getCOG().getID()+"."+task.getID();
    }
    
}
