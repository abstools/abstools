package abs.backend.java.lib.runtime;

import java.util.List;

import abs.backend.java.lib.runtime.SimpleTaskScheduler.TaskInfo;
import abs.backend.java.observing.TaskSchedulerView;

public class RecordingSchedulerStrategy implements SchedulingStrategy {

    private final SchedulingStrategy schedulingStrat;

    public RecordingSchedulerStrategy(SchedulingStrategy s) {
        this.schedulingStrat = s;
    }
    
    @Override
    public TaskInfo schedule(TaskScheduler scheduler, List<TaskInfo> scheduableTasks) {
        int cogId = scheduler.getCOG().getID();
        
        TaskInfo choosenTask = schedulingStrat.schedule(scheduler, scheduableTasks);
        long taskId = choosenTask.id;
        System.out.println("scheduled in COG "+cogId+" ("+scheduler.getCOG().getInitialClass().getName()+") task "+taskId+" from {"+tasksToStringList(cogId,scheduableTasks)+"}");
        return choosenTask;
    }

    private String tasksToStringList(int cogid, List<TaskInfo> scheduableTasks) {
        StringBuilder res = new StringBuilder();
        boolean first = true;
        for (TaskInfo info : scheduableTasks) {
            if (first) first = false;
            else res.append(", ");
            res.append(info.id);
        }
        
        return res.toString();
    }

    

}
