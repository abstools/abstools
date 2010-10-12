package abs.backend.java.lib.runtime;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Random;
import java.util.concurrent.atomic.AtomicLong;
import java.util.logging.Logger;

import abs.backend.java.observing.TaskSchedulerView;

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
    
    protected final SchedulingStrategy schedulingStrategy;
    
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
    
    
    public SimpleTaskScheduler(COG cog, SchedulingStrategy strat) {
        this.cog = cog;
        this.schedulingStrategy = strat; 
    }

    protected synchronized void taskFinished() {
        activeTask = null;
        schedule();
    }
    
    @Override
    public synchronized void addTask(Task<?> task) {
        readyTasks.add(new TaskInfo(task));
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
        ABSRuntime.scheduleTask(cog);
        
        List<TaskInfo> suspendedTasksWithSatisfiedGuards = unsuspendTasks();
        List<TaskInfo> choices = new ArrayList<TaskInfo>(readyTasks);
        choices.addAll(suspendedTasksWithSatisfiedGuards);

        if (choices.isEmpty())
            return;
        
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

    @Override
    public TaskSchedulerView getView() {
        // TODO Auto-generated method stub
        return null;
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
            
            newTask = activeTask;
        }
        
        if (newTask != currentTask) {
            thread.await();
        }
    }

    
    static final SchedulingStrategy defaultStrategy;
    static {
        String s = System.getProperty("abs.schedulingstrategy","default");
        SchedulingStrategy strat = null;
        if (s.equals("default") || s.equals("random")) {
            strat = new RandomTaskSchedulingStrategy();
        } else {
            logger.warning("Scheduling strategy "+s+" does not exist, falling back to default strategy");
            strat = new RandomTaskSchedulingStrategy();
        }
        
        logger.info("Using scheduling strategy '"+s+"'");
        
        boolean recording = Boolean.parseBoolean(System.getProperty("abs.recordscheduling","false"));
        if (recording) {
            strat = new RecordingSchedulerStrategy(strat);
            logger.info("Recording schedule");
        }
        
        defaultStrategy = strat;
    }
    
    public static TaskSchedulerFactory getFactory() {
        return new TaskSchedulerFactory() {
            @Override
            public TaskScheduler createTaskScheduler(COG cog) {
                return new SimpleTaskScheduler(cog,defaultStrategy);
            }
        };
    }


    @Override
    public COG getCOG() {
        return cog;
    }
}
