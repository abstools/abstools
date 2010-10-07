package abs.backend.java.lib.runtime;

import abs.backend.java.debugging.Debugger;
import abs.backend.java.observing.SystemObserver;


public class ABSRuntime {
    private static final SystemObserver systemObserver = (SystemObserver) loadClassByProperty("abs.systemobserver");
    private static final Debugger debugger = (Debugger) loadClassByProperty("abs.debugger");;
    public static final boolean DEBUGGING = debugger != null;
    
    public static void nextStep(String fileName, int line) {
        Debugger d = debugger;
        if (d != null) {
            d.nextStep(getCurrentTask().getView(), fileName, line);
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
    
    static Task getCurrentTask() {
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
	
	public static ABSFut asyncCall(Task<?> task) {
	    task.schedule();
	    return task.getFut();
	}
}
