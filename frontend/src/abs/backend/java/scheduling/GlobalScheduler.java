package abs.backend.java.scheduling;

import java.util.logging.Logger;

import abs.backend.java.lib.runtime.Logging;
import abs.backend.java.lib.runtime.Task;

public class GlobalScheduler {
	 private static Logger logger = Logging.getLogger(GlobalScheduler.class.getName());
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
                
            logger.finest("Choose next action...");
            ScheduleAction next = strategy.choose(options);
            logger.finest("Action "+next+" choosen");
            options.removeOption(next);
            logger.finest("Executing Action "+next);
            next.execute();
            logger.finest("Action "+next+" was executed.");
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

    public synchronized void addAction(ScheduleAction action) {
        options.addOption(action);
    }
    
    
}
