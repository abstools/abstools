package abs.backend.java.scheduling;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Random;
import java.util.concurrent.atomic.AtomicLong;
import java.util.logging.Logger;

import abs.backend.java.lib.runtime.ABSGuard;
import abs.backend.java.lib.runtime.ABSRuntime;
import abs.backend.java.lib.runtime.ABSThread;
import abs.backend.java.lib.runtime.COG;
import abs.backend.java.lib.runtime.Logging;
import abs.backend.java.lib.runtime.Task;
import abs.backend.java.observing.SystemObserver;
import abs.backend.java.observing.TaskObserver;
import abs.backend.java.observing.TaskSchedulerView;
import abs.backend.java.observing.TaskView;

/**
 * A very simple scheduler that is not the most efficient one, but
 * is easy to understand and also makes it easy to override the scheduling behavior
 * 
 * @author Jan Schäfer
 *
 */
public class SimpleTaskScheduler implements TaskScheduler {
    private final AtomicLong idCounter = new AtomicLong();
    static Logger logger = Logging.getLogger("scheduler");
    
    public class TaskInfo {
        /**
         * A COG-wide unique id, that is increment for each new task.
         */
        public long id = idCounter.incrementAndGet();
        
        public final Task<?> task;
        
        /**
         * is null unless this task as a fixed assigned thread
         */
        public SimpleSchedulerThread thread;
        
        /**
         * is null unless this task is waiting on a guard
         * suspended tasks are always waiting on a guard
         */
        public ABSGuard guard;
        
        public TaskInfo(Task<?> task) {
            this.task = task;
        }
        
        public boolean isSuspended() {
            return guard != null;
        }

        public void makeReady() {
            guard = null;
        }

        public void suspend(ABSGuard g) {
            guard = g;
        }
    }
    
    protected final TaskSchedulingStrategy schedulingStrategy;
    
    /**
     * Holds a list of all ready tasks.
     * Ready tasks are tasks that can definitely be scheduled.
     * Note that this list does not include tasks that are suspended on an unstable guard,
     * i.e., a guard that may become false after it was true. Such tasks are in the
     * suspendedTasks list.
     * 
     * Invariant: for all task in readyTasks: task.isSuspended() == false
     */
    protected final List<TaskInfo> readyTasks = new ArrayList<TaskInfo>();
    
    /**
     * Holds a list of all suspended tasks
     * Invariant: for all task in suspendedTasks: task.isSuspended() == true
     */
    protected final List<TaskInfo> suspendedTasks = new ArrayList<TaskInfo>();
    
    /**
     * Holds the currently active task or is null if there is no active task
     */
    protected TaskInfo activeTask;

    protected final COG cog;
    
    public SimpleTaskScheduler(COG cog) {
        this(cog, RandomTaskSchedulingStrategy.INSTANCE);
    }
    
    
    public SimpleTaskScheduler(COG cog, TaskSchedulingStrategy strat) {
        this.cog = cog;
        this.schedulingStrategy = strat; 
    }

    protected synchronized void taskFinished() {
        activeTask = null;
        schedule();
        ABSRuntime.doNextStep();
    }
    
    @Override
    public synchronized void addTask(Task<?> task) {
        readyTasks.add(new TaskInfo(task));
        if (view != null)
            view.taskAdded(task.getView());
        
        if (activeTask == null) {
            schedule();
        }
    }

    class SimpleSchedulerThread extends ABSThread {

        private final TaskInfo executingTask;

        public SimpleSchedulerThread(TaskInfo activeTask) {
            this.executingTask = activeTask;
            setName("ABS Scheduler Thread of "+cog.toString());
            setCOG(cog);
        }
        
        @Override
        public void run() {
            View v = view;
            if (v != null)
                v.taskStarted(executingTask.task.getView());
            
            executingTask.task.run();
            taskFinished();
        }

        public synchronized void await() {
            try {
                wait();
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }

        public synchronized void awake() {
            notify();
        }
        
    }
    
    private synchronized void schedule() {
        if (ABSRuntime.GLOBAL_SCHEDULING) {
            if (suspendedTasks.isEmpty() && readyTasks.isEmpty())
                return;
            
            ABSRuntime.addScheduleAction(new ScheduleTask(cog) {
                @Override
                public synchronized void execute() {
                    doSchedule();
                }
            });
        } else {
            doSchedule();
        }
    }
    
    private synchronized void doSchedule() {
        List<TaskInfo> suspendedTasksWithSatisfiedGuards = unsuspendTasks();
        List<TaskInfo> choices = new ArrayList<TaskInfo>(readyTasks);
        choices.addAll(suspendedTasksWithSatisfiedGuards);

        if (choices.isEmpty()) {
            ABSRuntime.doNextStep();
            return;
        }
        
        TaskInfo nextTask = schedule(choices);
        
        if (nextTask.isSuspended()) {
            suspendedTasks.remove(nextTask);
            nextTask.makeReady();
        } else {
            readyTasks.remove(nextTask);
        }
        activeTask = nextTask;
        if (activeTask.thread != null) {
            activeTask.thread.awake();
        } else {
            activeTask.thread = new SimpleSchedulerThread(activeTask);
            activeTask.thread.start();
        }
        
    }
    
    private synchronized List<TaskInfo> unsuspendTasks() {
        List<TaskInfo> tasksWithSatisfiedGuards = new ArrayList<TaskInfo>(0);
        
        Iterator<TaskInfo> it = suspendedTasks.iterator();
        while (it.hasNext()) {
            TaskInfo task = it.next();
            if (task.guard.isTrue()) {
                if (task.guard.staysTrue()) {
                    readyTasks.add(task);
                    task.makeReady();
                    it.remove();
                } else {
                    tasksWithSatisfiedGuards.add(task);
                }
            }
        }
        return tasksWithSatisfiedGuards;
    }

    protected TaskInfo schedule(List<TaskInfo> scheduableTasks) {
        return schedulingStrategy.schedule(this, scheduableTasks);
    }

    private volatile View view;
    
    @Override
    public synchronized TaskSchedulerView getView() {
        if (view == null)
            view = new View();
        return view;
    }

    @Override
    public synchronized Task<?> getActiveTask() {
        return activeTask.task;
    }

    @Override
    public void await(ABSGuard g) {
        SimpleSchedulerThread thread = null;
        TaskInfo newTask = null;
        TaskInfo currentTask = null;
        
        synchronized (this) {
            currentTask = activeTask;
            thread = currentTask.thread;
            currentTask.suspend(g);
            suspendedTasks.add(currentTask);
            activeTask = null;
            
            schedule();
            ABSRuntime.doNextStep();
            newTask = activeTask;
        }
        
        if (newTask != currentTask) {
            thread.await();
        }
    }

    
    public static TaskSchedulerFactory getFactory() {
        return new TaskSchedulerFactory() {
            @Override
            public TaskScheduler createTaskScheduler(COG cog) {
                return new SimpleTaskScheduler(cog,ABSRuntime.taskSchedulingStrategy);
            }
        };
    }


    @Override
    public COG getCOG() {
        return cog;
    }
    
    private class View extends  AbstractTaskSchedulerView {

        @Override
        public List<TaskView> getNewTasks() {
            return null;
        }

        @Override
        public List<TaskView> getSuspendedTasks() {
            return null;
        }

        @Override
        public TaskView getActiveTask() {
            return SimpleTaskScheduler.this.getActiveTask().getView();
        }

    }
}
