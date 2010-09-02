package abs.backend.java.lib.runtime;

import abs.backend.java.observing.SystemObserver;


public class ABSRuntime {
    private static SystemObserver systemObserver = null;
    
    public static void setSystemObserver(SystemObserver s) {
        systemObserver = s;
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
            systemObserver.systemStarted(getCurrentTask().getView(), getCurrentCOG().getView());
        }
    }
    
    private static Task getCurrentTask() {
        return getCurrentCOG().getScheduler().getActiveTask();
    }

    public static void suspend() {
        await(new ABSTrueGuard());
    }
    
    public static void await(ABSGuard g) {
        getCurrentCOG().getScheduler().await(g);
	}

	public static COG getCurrentCOG() {
	   return getCurrentThread().getCOG();
   }
	
	public static ABSThread getCurrentThread() {
		return (ABSThread) Thread.currentThread();
	}
	
	public static ABSFut asyncCall(Task<?> task) {
	    task.schedule();
	    return task.getFut();
	}
}
