package abs.backend.java.lib.runtime;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.logging.Logger;

import abs.backend.java.observing.SchedulerView;
import abs.backend.java.observing.TaskListener;
import abs.backend.java.observing.TaskView;

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
        if (view != null)
            view.taskAdded(task.getView());
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
                    if (view != null)
                        view.taskStarted(runningTask.getView());
                    
                }
                if (Logging.DEBUG) log.finest("Executing "+runningTask);
                try {
                    runningTask.run();
                    synchronized (TaskScheduler.this) {
                        if (view != null)
                            view.taskFinished(runningTask.getView());
                    }
                    
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

    public synchronized Task<?> getActiveTask() {
        return activeTask;
    }

    private View view;
    
    public synchronized SchedulerView getView() {
        if (view == null) {
            view = new View();
        }
        return view;
    }
    
    private class View implements SchedulerView {
        private List<TaskListener> listeners;

        @Override
        public synchronized void registerTaskActionListener(TaskListener listener) {
            if (listeners == null) {
                listeners = new ArrayList<TaskListener>(1);
            }
            listeners.add(listener);
        }

        public void taskStarted(TaskView view) {
            if (listeners != null) {
                for (TaskListener l : listeners) {
                    l.taskStarted(view);
                }
            }
        }

        public synchronized void taskFinished(TaskView view) {
            if (listeners != null) {
                for (TaskListener l : listeners) {
                    l.taskFinished(view);
                }
            }
        }

        public synchronized void taskAdded(TaskView view) {
            if (listeners != null) {
                for (TaskListener l : listeners) {
                    l.taskCreated(view);
                }
            }
        }

        @Override
        public List<TaskView> getNewTasks() {
            // TODO Auto-generated method stub
            return null;
        }

        @Override
        public List<TaskView> getSuspendedTasks() {
            // TODO Auto-generated method stub
            return null;
        }

        @Override
        public TaskView getActiveTask() {
            return TaskScheduler.this.getActiveTask().getView();
        }
        
    }
}
