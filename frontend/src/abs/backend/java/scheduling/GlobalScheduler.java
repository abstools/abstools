package abs.backend.java.scheduling;

import abs.backend.java.lib.runtime.Task;

public class GlobalScheduler {
    private final ScheduleOptions options = new ScheduleOptions();
    private final GlobalSchedulingStrategy strategy;
    
    public GlobalScheduler(GlobalSchedulingStrategy strategy) {
        this.strategy = strategy;
    }
    
    private long totalNumChoices = 0;
    
    public void doNextScheduleStep() {
        synchronized (this) {
            if (options.isEmpty()) {
                System.out.println("No steps left. Program finished");
                System.out.println("Total number of global choices: "+totalNumChoices);
                if (totalNumChoices == 0) {
                    System.out.println("Program is deterministic!");
                }
                return;
            }
            
            totalNumChoices += options.numOptions()-1;
                
            ScheduleAction next = strategy.choose(options);
            options.removeOption(next);
            System.out.println("Executing "+next);
            next.execute();
        }
    }
    
    public void stepTask(Task<?> task) {
        ScheduleAction a = new StepTask(task);

        synchronized (this) {
            options.addOption(a);
            doNextScheduleStep();
        }
        
        a.await();
    }

    public void addAction(ScheduleAction action) {
        options.addOption(action);
    }
    
    
}
