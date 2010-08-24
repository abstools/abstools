package abs.backend.java.lib.runtime;

import java.util.LinkedList;
import java.util.List;

class TaskScheduler {
    private final List<Task> taskq = new LinkedList<Task>();
    private final List<SchedulerThread> suspendedTasks = new LinkedList<SchedulerThread>();
    private Task activeTask;
    private SchedulerThread thread;
    private final COG cog;
    
    public TaskScheduler(COG cog) {
        this.cog = cog;
    }
    
	public synchronized void suspend() {
		if (thread != null) {
		    thread.suspendTask();
		}
	}

    public synchronized void addTask(Task task) {
        taskq.add(task);
        if (thread == null) {
            thread = new SchedulerThread();
            thread.start();
        } else {
            notify();
        }
    }
    
    class SchedulerThread extends ABSThread {
        private Task runningTask;

        public SchedulerThread() {
            setName("ABS Task Thread of COG "+cog.getID());
            setCOG(cog);
        }
        
        @Override
        public void run() {
            
            while (true) {
                synchronized (TaskScheduler.this) {
                    activeTask = null;
                    if (taskq.isEmpty()) {
                        TaskScheduler.this.thread = null;
                        return;
                    }
                    
                    activeTask = taskq.remove(0);
                    runningTask = activeTask;
                }
                
                runningTask.run();
            }
        }

        // assume called in synchronized block 
        public void suspendTask() {
            // TODO: implement suspend
        }
    }
}
