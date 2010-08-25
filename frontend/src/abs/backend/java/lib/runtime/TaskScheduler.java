package abs.backend.java.lib.runtime;

import java.util.LinkedList;
import java.util.List;
import java.util.logging.Logger;

class TaskScheduler {
    private static final Logger log = Logging.getLogger(TaskScheduler.class.getName());
    
    private final List<Task<?>> newTasks = new LinkedList<Task<?>>();
    private final List<SchedulerThread> suspendedTasks = new LinkedList<SchedulerThread>();
    private Task<?> activeTask;
    private SchedulerThread thread;
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
            setName("ABS Task Thread of COG "+cog.getID());
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
                }
                log.finest("Running task "+runningTask.methodName()+" of COG "+cog.getID());
                runningTask.run();
            }
        }

        // assume called in synchronized block 
        public void suspendTask(ABSGuard g) {
            activeTask = null; 
            thread = null;
            suspendedTasks.add(this);
            TaskScheduler.this.notifyAll();
            while (!g.isTrue()) {
               try {
                   wait();
               } catch (InterruptedException e) {
                   e.printStackTrace();
               }
            }
            thread = this;
            activeTask = runningTask;
        }
    }

    public synchronized void await(ABSGuard g) {
        thread.suspendTask(g);
    }
}
