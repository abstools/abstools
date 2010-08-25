package abs.backend.java.lib.runtime;


public class ABSRuntime {

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
