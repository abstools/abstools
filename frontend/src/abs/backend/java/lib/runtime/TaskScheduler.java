package abs.backend.java.lib.runtime;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.logging.Logger;

import abs.backend.java.observing.SchedulerView;
import abs.backend.java.observing.TaskObserver;
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
        log.finest(task+" ADDED TO QUEUE");
        
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
                
                View v = view;
                if (v != null)
                    v.taskStarted(runningTask.getView());
                

                if (Logging.DEBUG) log.finest("Executing "+runningTask);
                try {
                    runningTask.run();
                    v = view;
                    if (v != null)
                        v.taskFinished(runningTask.getView());
                    
                    
                    if (Logging.DEBUG) log.finest("Task "+runningTask+" FINISHED");
                } catch(Exception e) {
                    if (Logging.DEBUG) log.finest("EXCEPTION in Task "+runningTask);
                    e.printStackTrace();
                }
            }
        }

        // assume called in synchronized block 
        public void suspendTask(ABSGuard g) {
            if (Logging.DEBUG) log.finest(runningTask+" on "+g+" SUSPENDING");
            synchronized (TaskScheduler.this) {
                activeTask = null; 
                thread = null;
                if (!newTasks.isEmpty()) {
                   log.finest(runningTask+" on "+g+" Starting new Scheduler Thread");
               	 
                    thread = new SchedulerThread();
                    thread.start();
                } else {
                    TaskScheduler.this.notifyAll();
                }
                if (Logging.DEBUG) log.finest(runningTask+" on "+g+" SUSPENDING");
                suspendedTasks.add(this);
            }
            
            View v = view;
            if (v != null) {
                v.taskSuspended(runningTask.getView(), g);
            }
            
            if (Logging.DEBUG) log.finest(runningTask+" AWAITING "+g);
            boolean couldBecomeFalse = g.await();
            
            if (!couldBecomeFalse) {
                if (Logging.DEBUG) log.finest(runningTask+" "+g+" READY");
                if (v != null)
                    v.taskReady(runningTask.getView());
            }
            
            synchronized (TaskScheduler.this) {
                while (! (g.isTrue() && thread == null)) {
                    try {
                       log.finest(runningTask+" "+g+" WAITING FOR WAKE UP");
                        TaskScheduler.this.wait();
                        if (Logging.DEBUG)
                            log.finest(runningTask+" WOKE UP...");
                    } catch (InterruptedException e) {
                        e.printStackTrace();
                    }
                }
                thread = this;
                activeTask = runningTask;
                suspendedTasks.remove(this);
            }
            
            if (v != null)
                v.taskResumed(runningTask.getView(), g);
            
            if (Logging.DEBUG) log.finest(runningTask+" "+g+" ACTIVE");
        }
    }

    public void await(ABSGuard g) {
        thread.suspendTask(g);
    }

    public synchronized Task<?> getActiveTask() {
        return activeTask;
    }

    private volatile View view;
    
    public synchronized SchedulerView getView() {
        if (view == null) {
            view = new View();
        }
        return view;
    }
    
    private class View implements SchedulerView {
        private List<TaskObserver> observers;

        @Override
        public synchronized void registerTaskObserver(TaskObserver listener) {
            getObservers().add(listener);
        }

        public void taskReady(TaskView view) {
            for (TaskObserver l : getObservers()) {
                l.taskReady(view);
            }
        }

        synchronized void taskResumed(TaskView runningTask, ABSGuard g) {
            for (TaskObserver l : getObservers()) {
                l.taskResumed(runningTask, g.getView());
            }
        }

        synchronized List<TaskObserver> getObservers() {
            if (observers == null) {
                observers = new ArrayList<TaskObserver>(1);
            }
            return observers;
        }
        
        public synchronized void taskSuspended(TaskView runningTask, ABSGuard g) {
            for (TaskObserver l : getObservers()) {
                l.taskSuspended(runningTask, g.getView());
            }
        }

        public void taskStarted(TaskView view) {
            for (TaskObserver l : getObservers()) {
                l.taskStarted(view);
            }
        }

        public synchronized void taskFinished(TaskView view) {
            for (TaskObserver l : getObservers()) {
                l.taskFinished(view);
            }
        }

        public synchronized void taskAdded(TaskView view) {
            for (TaskObserver l : getObservers()) {
                l.taskCreated(view);
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
