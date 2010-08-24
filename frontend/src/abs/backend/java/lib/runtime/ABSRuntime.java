package abs.backend.java.lib.runtime;

import abs.backend.java.lib.types.ABSGuard;

public class ABSRuntime {

    public static void await(ABSGuard g) {
		while (!g.isTrue()) {
			g.await();
		}
	}

	public static void suspend() {
		getCurrentCOG().getScheduler().suspend();
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
