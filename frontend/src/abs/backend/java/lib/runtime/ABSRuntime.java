package abs.backend.java.lib.runtime;

import java.util.logging.Logger;

import abs.backend.java.debugging.Debugger;
import abs.backend.java.observing.SystemObserver;
import abs.backend.java.scheduling.DefaultTaskScheduler;
import abs.backend.java.scheduling.GlobalScheduler;
import abs.backend.java.scheduling.GlobalSchedulingStrategy;
import abs.backend.java.scheduling.RandomGlobalSchedulingStrategy;
import abs.backend.java.scheduling.RandomTaskSchedulingStrategy;
import abs.backend.java.scheduling.RecordingSchedulerStrategy;
import abs.backend.java.scheduling.ScheduleAction;
import abs.backend.java.scheduling.ScheduleOptions;
import abs.backend.java.scheduling.ScheduleTask;
import abs.backend.java.scheduling.SimpleTaskScheduler;
import abs.backend.java.scheduling.TaskSchedulerFactory;
import abs.backend.java.scheduling.TaskSchedulingStrategy;
import abs.backend.java.scheduling.TotalSchedulingStrategy;


public class ABSRuntime {
    private static final Logger logger = Logging.getLogger("runtime");
    private static final SystemObserver systemObserver = (SystemObserver) loadClassByProperty("abs.systemobserver");
    public static final boolean DEBUGGING = System.getProperty("abs.debug","false").equals("true");
    
    public static final TotalSchedulingStrategy totalSchedulingStrategy;
    public static final GlobalScheduler globalScheduler;
    public static final TaskSchedulingStrategy taskSchedulingStrategy;
    public static final TaskSchedulerFactory taskSchedulerFactory;

    private static boolean configuredTaskScheduling = false;
    
    static {
        TotalSchedulingStrategy strat = (TotalSchedulingStrategy) loadClassByProperty("abs.totalscheduler");
        if (strat != null) {
            logger.info("Using total scheduling strategy defined by class "+strat.getClass().getName());
            configuredTaskScheduling = true;
        } 
        totalSchedulingStrategy = strat;
    }

    static {
        TaskSchedulingStrategy strat = (TaskSchedulingStrategy) loadClassByProperty("abs.taskschedulerstrategy");
        if (strat == null)
            strat = totalSchedulingStrategy;
        else
            configuredTaskScheduling = true;
        
        if (strat == null) {
            strat = new RandomTaskSchedulingStrategy();
        }
        
        logger.info("Using task scheduling strategy defined by class "+strat.getClass().getName());
        
        boolean recording = Boolean.parseBoolean(System.getProperty("abs.recordscheduling","false"));
        if (recording) {
            strat = new RecordingSchedulerStrategy(strat);
            logger.info("Recording schedule");
        }

        taskSchedulingStrategy = strat;
    }
    
    static {
        GlobalSchedulingStrategy globalSchedulerStrat = (GlobalSchedulingStrategy) loadClassByProperty("abs.globalscheduler");
        if (globalSchedulerStrat == null) 
            globalSchedulerStrat = totalSchedulingStrategy;
        
        if (globalSchedulerStrat != null) {
            logger.info("Using global scheduling strategy defined by class "+globalSchedulerStrat.getClass().getName());
            globalScheduler = new GlobalScheduler(globalSchedulerStrat);
        } else {
            globalScheduler = null;
        }
    }
    
    static {
        taskSchedulerFactory = getSchedulerFactory();
    }
    private static TaskSchedulerFactory getSchedulerFactory() {
        // only the SimpleTaskScheduler supports configuring
        if (configuredTaskScheduling) {
            logger.info("Using simple task scheduler, because task scheduling is specified");
            return SimpleTaskScheduler.getFactory();
        }
        
        String schedulerName = System.getProperty("abs.taskscheduler","default");
        System.out.println("Scheduler: "+schedulerName);
        if (schedulerName.equals("default"))
            return DefaultTaskScheduler.getFactory();
        else if (schedulerName.equals("simple"))
            return SimpleTaskScheduler.getFactory();
        System.err.println("The task scheduler "+schedulerName+" does not exist, falling back to the default task scheduler.");
        return DefaultTaskScheduler.getFactory();
    }

    
    

    public static final boolean GLOBAL_SCHEDULING = globalScheduler != null;

    public static void scheduleTaskDone() {
        if (GLOBAL_SCHEDULING)
            globalScheduler.doNextScheduleStep();
    }

    
    
    public static void nextStep(String fileName, int line) {
        if (DEBUGGING) {
            getCurrentTask().nextStep(fileName,line);
        } 

        if (GLOBAL_SCHEDULING) {
            globalScheduler.stepTask(getCurrentTask());
        }
        
    }
    
    public static void addScheduleAction(ScheduleAction action) {
        if (GLOBAL_SCHEDULING) {
            globalScheduler.addAction(action);
        }
    }

    public static void doNextStep() {
        if (GLOBAL_SCHEDULING) {
            globalScheduler.doNextScheduleStep();
        }
    }

    
    
    
    public static void cogCreated(ABSObject o) {
        if (systemObserver != null) {
            systemObserver.newCOGCreated(o.getCOG().getView(), o.getView());
        }
    }
    
    
    public static void systemStarted() {
        if (systemObserver != null) {
            systemObserver.systemStarted();
        }
    }

    public static Object loadClassByProperty(String property) {
        try {
            String s = System.getProperty(property);
            if (s != null) {
                Class<?> clazz = ABSRuntime.class.getClassLoader().loadClass(s);
                return clazz.newInstance();
            }
        } catch (ClassNotFoundException e) {
            e.printStackTrace();
        } catch (InstantiationException e) {
            e.printStackTrace();
        } catch (IllegalAccessException e) {
            e.printStackTrace();
        }
        return null;
    }
    
    static Task<?> getCurrentTask() {
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
	   else
	       return null;
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
