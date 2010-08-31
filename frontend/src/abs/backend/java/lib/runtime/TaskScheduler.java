package abs.backend.java.lib.runtime;

import java.util.LinkedList;
import java.util.List;
import java.util.logging.Logger;

class TaskScheduler {
    private static final Logger log = Logging.getLogger(ABSRuntime.class.getName());
    
    private final List<Task<?>> newTasks = new LinkedList<Task<?>>();
    private final List<SchedulerThread> suspendedTasks = new LinkedList<SchedulerThread>();
    private Task<?> activeTask;
    private volatile SchedulerThread thread;
    private final COG cog;
    
    public TaskScheduler(COG cog) {
        this.cog = cog;
    }
    
    public synchronized void addTask(Task<?> task) {
        newTasks.add(task);
        if (thread == null) {
            thread = new SchedulerThread();
            thread.start();
        } else {
            notifyAll();
        }
    }
    
    class SchedulerThread extends ABSThread {
        private Task<?> runningTask;

        public SchedulerThread() {
            setName("ABS Scheduler Thread of "+cog.toString());
            setCOG(cog);
        }
        
        @Override
        public void run() {
            
            while (true) {
                synchronized (TaskScheduler.this) {
                    activeTask = null;
                    if (newTasks.isEmpty()) {
                        thread = null;
                        TaskScheduler.this.notifyAll();
                        return;
                    }
                    
                    activeTask = newTasks.remove(0);
                    runningTask = activeTask;
                    setName("ABS Scheduler Thread executing "+activeTask.toString());
                    
                }
                if (Logging.DEBUG) log.finest("Executing "+runningTask);
                try {
                    runningTask.run();
                    if (Logging.DEBUG) log.finest("Task "+runningTask+" FINISHED");
                } catch(Exception e) {
                    if (Logging.DEBUG) log.finest("EXCEPTION in Task "+runningTask);
                    e.printStackTrace();
                }
            }
        }

        // assume called in synchronized block 
        public void suspendTask(ABSGuard g) {
            synchronized (TaskScheduler.this) {
                activeTask = null; 
                thread = null;
                if (!newTasks.isEmpty()) {
                    thread = new SchedulerThread();
                    thread.start();
                } else {
                    TaskScheduler.this.notifyAll();
                }
                if (Logging.DEBUG) log.finest(runningTask+" on "+g+" SUSPENDING");
                suspendedTasks.add(this);
            }
            
            if (Logging.DEBUG) log.finest(runningTask+" AWAITING "+g);
            g.await();
            if (Logging.DEBUG) log.finest(runningTask+" "+g+" READY");
            
            synchronized (TaskScheduler.this) {
                while (! (g.isTrue() && thread == null)) {
                    try {
                        TaskScheduler.this.wait();
                        if (Logging.DEBUG)
                            log.finest(runningTask+" WOKE UP...");
                    } catch (InterruptedException e) {
                        e.printStackTrace();
                    }
                }
                thread = this;
                activeTask = runningTask;
            }
        }
    }

    public void await(ABSGuard g) {
        thread.suspendTask(g);
    }
}
