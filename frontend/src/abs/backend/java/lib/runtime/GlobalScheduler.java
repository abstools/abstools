package abs.backend.java.lib.runtime;

import abs.backend.java.scheduling.GlobalSchedulingStrategy;
import abs.backend.java.scheduling.ScheduleAction;
import abs.backend.java.scheduling.ScheduleOptions;
import abs.backend.java.scheduling.ScheduleTask;
import abs.backend.java.scheduling.StepTask;

public class GlobalScheduler {
    private final ScheduleOptions options = new ScheduleOptions();
    private final GlobalSchedulingStrategy strategy;
    private final GlobalSchedulerThread schedulerThread = new GlobalSchedulerThread();
    
    public GlobalScheduler(GlobalSchedulingStrategy strategy) {
        this.strategy = strategy;
        schedulerThread.start();
    }
    
    public void stepTask(Task<?> task) {
        awaitAction(new StepTask(task));
    }
    
    public void scheduleTask(COG cog) {
        awaitAction(new ScheduleTask(cog));
    }
    
    boolean scheduleNext = false;
    private void awaitAction(ScheduleAction a) {
        synchronized (this) {
            options.addOption(a);
            scheduleNext = true;
            notify();
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
                        while (!scheduleNext) {
                            GlobalScheduler.this.wait();
                        }
                        scheduleNext = false;
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
    
    
}
