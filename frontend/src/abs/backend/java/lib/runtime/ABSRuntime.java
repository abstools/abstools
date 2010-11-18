package abs.backend.java.lib.runtime;

import java.util.Random;
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
    private static final Logger logger = Logging.getLogger(ABSRuntime.class.getName());

    private static final SystemObserver systemObserver = Config.systemObserver;
    
    public static final TotalSchedulingStrategy totalSchedulingStrategy = Config.totalSchedulingStrategy;
    public static final GlobalScheduler globalScheduler = Config.globalScheduler;
    public static final TaskSchedulingStrategy taskSchedulingStrategy = Config.taskSchedulingStrategy;
    public static final TaskSchedulerFactory taskSchedulerFactory = Config.taskSchedulerFactory;
    
    
    public static void scheduleTaskDone() {
        if (Config.GLOBAL_SCHEDULING)
            globalScheduler.doNextScheduleStep();
    }


	public static void nextStep(String fileName, int line) {
        if (Config.DEBUGGING) {
            getCurrentTask().nextStep(fileName,line);
        } 

        if (Config.GLOBAL_SCHEDULING) {
            globalScheduler.stepTask(getCurrentTask());
        }
        
    }
    
	public static <O> O checkForNull(O o) {
	    if (o == null) {
	        throw new ABSNullPointerException();
	    } 
	    return o;
	}
	
    public static void addScheduleAction(ScheduleAction action) {
        if (Config.GLOBAL_SCHEDULING) {
            globalScheduler.addAction(action);
        }
    }

    public static void doNextStep() {
        if (Config.GLOBAL_SCHEDULING) {
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


	public static GlobalScheduler getGlobalScheduler() {
	    return globalScheduler;
	}
	


}
