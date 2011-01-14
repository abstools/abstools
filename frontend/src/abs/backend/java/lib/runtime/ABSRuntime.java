package abs.backend.java.lib.runtime;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import java.util.logging.Logger;

import abs.backend.java.observing.SystemObserver;
import abs.backend.java.scheduling.DefaultTaskScheduler;
import abs.backend.java.scheduling.GlobalScheduler;
import abs.backend.java.scheduling.GlobalSchedulingStrategy;
import abs.backend.java.scheduling.RandomSchedulingStrategy;
import abs.backend.java.scheduling.ScheduleAction;
import abs.backend.java.scheduling.TaskScheduler;
import abs.backend.java.scheduling.TaskSchedulerFactory;
import abs.backend.java.scheduling.TaskSchedulingStrategy;
import abs.backend.java.scheduling.TotalSchedulingStrategy;

public class ABSRuntime {
    private static final Logger logger = Logging.getLogger(ABSRuntime.class.getName());

    private static final ThreadLocal<COG> currentCOG = new ThreadLocal<COG>();
    
    
    private final ABSThreadManager threadManager = new ABSThreadManager();

    public void start(Class<? extends ABSObject> mainClass) throws InstantiationException, IllegalAccessException {
        systemStarted();
        COG cog = new COG(this,mainClass);
        currentCOG.set(cog);
        ABSObject mainObject = mainClass.newInstance();
        cogCreated(mainObject);
        asyncCall(new ABSMainTask(mainObject));
        doNextStep();
    }
    
    private final List<SystemObserver> systemObserver = new ArrayList<SystemObserver>();

    private GlobalSchedulingStrategy globalSchedulingStrategy;
    private GlobalScheduler globalScheduler;
    private TaskSchedulingStrategy taskSchedulingStrategy;
    private TaskSchedulerFactory taskSchedulerFactory = DefaultTaskScheduler.getFactory();
    private volatile boolean debugging = false;
    private long randomSeed;
    private Random random;
    

    public synchronized void addSystemObserver(List<SystemObserver> systemObserver) {
        this.systemObserver.addAll(systemObserver);
    }

    public synchronized long getRandomSeed() {
        return randomSeed;
    }
    
    public void enableDebugging(boolean debug) {
        debugging = debug;
    }
    
    public boolean debuggingEnabled() {
        return debugging;
    }

    public synchronized void setRandomSeed(long seed) {
        randomSeed = seed;
        random = new Random(seed);
        logger.config("New Random Seed: " + randomSeed);
        
        if (globalSchedulingStrategy instanceof RandomSchedulingStrategy) {
            ((RandomSchedulingStrategy)globalSchedulingStrategy).setRandom(random);
        }

        if (taskSchedulingStrategy instanceof RandomSchedulingStrategy) {
            ((RandomSchedulingStrategy)taskSchedulingStrategy).setRandom(random);
        }
        
    }
    
    public synchronized Random getRandom() {
        return random;
    }
    
    public synchronized void setTotalSchedulingStrategy(TotalSchedulingStrategy strat) {
        setGlobalSchedulingStrategy(strat);
        setTaskSchedulingStrategy(strat);
    }

    public synchronized void setTaskSchedulingStrategy(TaskSchedulingStrategy strat) {
        taskSchedulingStrategy = strat;
    }
    
    public synchronized void setGlobalSchedulingStrategy(GlobalSchedulingStrategy strat) {
        globalSchedulingStrategy = strat;
        if (strat != null) {
            logger.config("Using global scheduling strategy defined by class "
                    + strat.getClass().getName());
            globalScheduler = new GlobalScheduler(this, strat);
        } else {
            globalScheduler = null;
        }
        
    }

    public synchronized void setTaskSchedulerFactory(TaskSchedulerFactory taskSchedulerFactory) {
        this.taskSchedulerFactory = taskSchedulerFactory;
    }

    public synchronized boolean hasGlobalScheduler() {
        return globalScheduler != null;
    }

    
    

    public void scheduleTaskDone() {
        if (hasGlobalScheduler()) {
            globalScheduler.doNextScheduleStep();
        }
    }

    public void nextStep(String fileName, int line) {
        if (debugging) {
            getCurrentTask().nextStep(fileName, line);
        }

        if (hasGlobalScheduler()) {
            globalScheduler.stepTask(getCurrentTask());
        }

    }

    public static <O> O checkForNull(O o) {
        if (o == null) {
            throw new ABSNullPointerException();
        }
        return o;
    }

    public void addScheduleAction(ScheduleAction action) {
        if (hasGlobalScheduler()) {
            globalScheduler.addAction(action);
        }
    }

    public void doNextStep() {
        if (hasGlobalScheduler()) {
            globalScheduler.doNextScheduleStep();
        }
    }

    public void cogCreated(ABSObject o) {
        if (!systemObserver.isEmpty()) {
            for (SystemObserver obs : systemObserver) {
                obs.newCOGCreated(o.getCOG().getView(), o.getView());
            }
        }
    }

    public void systemStarted() {
        if (!systemObserver.isEmpty()) {
            for (SystemObserver obs : systemObserver) {
                obs.systemStarted();
            }
        }
    }

    public GlobalScheduler getGlobalScheduler() {
        return globalScheduler;
    }
    
    public void shutdown() {
        threadManager.shutdownAllThreads();
    }

    public TaskScheduler createTaskScheduler(COG cog) {
        return taskSchedulerFactory.createTaskScheduler(this, cog, threadManager);
    }

    public TaskSchedulingStrategy getTaskSchedulingStrategy() {
        return taskSchedulingStrategy;
    }

    
    public static ABSRuntime getCurrentRuntime() {
        if (getCurrentCOG() != null) {
            return getCurrentCOG().getRuntime();
        } else {
            return null;
        }
    }
    
    public static Task<?> getCurrentTask() {
        if (getCurrentCOG() != null) {
            return getCurrentCOG().getScheduler().getActiveTask();
        } else {
            return null;
        }
    }

    public static void suspend() {
        await(new ABSTrueGuard());
    }

    public static void await(ABSGuard g) {
        getCurrentCOG().getScheduler().await(g);
    }

    public static COG getCurrentCOG() {
        final ABSThread thread = getCurrentThread();
        if (thread != null)
            return thread.getCOG();
        
        return currentCOG.get();
    }

    public static ABSThread getCurrentThread() {
        Thread currentThread = Thread.currentThread();
        if (currentThread instanceof ABSThread)
            return (ABSThread) currentThread;
        else
            return null;
    }

    public static ABSFut<?> asyncCall(Task<?> task) {
        task.schedule();
        return task.getFut();
    }

}
