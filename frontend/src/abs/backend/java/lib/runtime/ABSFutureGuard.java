package abs.backend.java.lib.runtime;



public class ABSFutureGuard extends ABSGuard {
	public final ABSFut fut;
	public ABSFutureGuard(ABSFut f) {
		this.fut = f;
	}
	
	public void await() {
		fut.await();
	}
	
	@Override
	public boolean isTrue() {
	   return fut.isResolved();
	}
	
	@Override
	public String toString() {
	    return "Future Guard on "+fut;
	}
}
