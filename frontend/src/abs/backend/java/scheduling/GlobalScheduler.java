package abs.backend.java.scheduling;

import abs.backend.java.lib.runtime.Task;

public class GlobalScheduler {
    private final ScheduleOptions options = new ScheduleOptions();
    private final GlobalSchedulingStrategy strategy;
    private final GlobalSchedulerThread schedulerThread = new GlobalSchedulerThread();
    
    public GlobalScheduler(GlobalSchedulingStrategy strategy) {
        this.strategy = strategy;
        schedulerThread.start();
    }
    
    private long totalNumChoices = 0;
    
    
    public void doNextScheduleStep() {
        synchronized (this) {
//            scheduleNext = true;
//            notify();
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
            next.execute();
            //System.out.println("Executed step "+next);
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
    
    
    class GlobalSchedulerThread extends Thread {
        
        public GlobalSchedulerThread() {
            setDaemon(true);
        }
        
        @Override
        public void run() {
            while (true) {
                synchronized (GlobalScheduler.this) {
                    try {
                        GlobalScheduler.this.wait();
                    } catch (InterruptedException e) {
                        e.printStackTrace();
                    }
                    ScheduleAction next = strategy.choose(options);
                    options.removeOption(next);
                    next.execute();
                }
            }
        }
    }


    public void addAction(ScheduleAction action) {
        options.addOption(action);
    }
    
    
}
