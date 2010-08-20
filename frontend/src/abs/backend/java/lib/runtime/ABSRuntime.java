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

	private static COG getCurrentCOG() {
	   return getCurrentThread().getCOG();
   }
	
	public static ABSThread getCurrentThread() {
		return (ABSThread) Thread.currentThread();
	}
}
