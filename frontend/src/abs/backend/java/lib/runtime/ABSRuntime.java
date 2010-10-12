package abs.backend.java.lib.runtime;

import abs.backend.java.debugging.Debugger;
import abs.backend.java.observing.SystemObserver;
import abs.backend.java.scheduling.GlobalSchedulingStrategy;
import abs.backend.java.scheduling.RandomGlobalSchedulingStrategy;
import abs.backend.java.scheduling.ScheduleOptions;


public class ABSRuntime {
    private static final SystemObserver systemObserver = (SystemObserver) loadClassByProperty("abs.systemobserver");
    private static final GlobalSchedulingStrategy scheduler = (GlobalSchedulingStrategy) loadClassByProperty("abs.globalscheduler");
    
    public static final boolean DEBUGGING = System.getProperty("abs.debug","false").equals("true");
    public static final boolean SCHEDULING = scheduler != null;
    
    public static final GlobalScheduler globalScheduler;
    public static final boolean GLOBAL_SCHEDULING;
    static {
        GLOBAL_SCHEDULING = Boolean.parseBoolean(System.getProperty("abs.useglobalscheduler","false"));
        if (GLOBAL_SCHEDULING) {
            globalScheduler = new GlobalScheduler(new RandomGlobalSchedulingStrategy());
        } else {
            globalScheduler = null;
        }
    }
    

    static final ScheduleOptions scheduleOptions = SCHEDULING ? new ScheduleOptions() : null;  
    
    public static void scheduleTask(COG cog) {
        if (GLOBAL_SCHEDULING)
            globalScheduler.scheduleTask(cog);
    }
    
    public static void nextStep(String fileName, int line) {
        if (GLOBAL_SCHEDULING) {
            globalScheduler.stepTask(getCurrentTask());
        }
        
        if (DEBUGGING) {
            getCurrentTask().nextStep(fileName,line);
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

    private static Object loadClassByProperty(String property) {
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
