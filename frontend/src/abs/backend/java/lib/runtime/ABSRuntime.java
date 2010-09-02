package abs.backend.java.lib.runtime;

import abs.backend.java.observing.SystemObserver;


public class ABSRuntime {
    private static volatile SystemObserver systemObserver = null;
    
    public static void setSystemObserver(SystemObserver s) {
        systemObserver = s;
    }
    
    
    public static void cogCreated(ABSObject o) {
        if (systemObserver != null) {
            systemObserver.newCOGCreated(o.getCOG().getView(), o.getView());
        }
    }
    
    
    public static void systemStarted() {
        String s = System.getProperty("abs.systemobserver");
        if (s != null) {
            try {
                Class<?> clazz = ABSRuntime.class.getClassLoader().loadClass(s);
                systemObserver = (SystemObserver) clazz.newInstance();
            } catch (ClassNotFoundException e) {
                e.printStackTrace();
            } catch (InstantiationException e) {
                e.printStackTrace();
            } catch (IllegalAccessException e) {
                e.printStackTrace();
            }
        }
        
        if (systemObserver != null) {
            systemObserver.systemStarted();
        }
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
