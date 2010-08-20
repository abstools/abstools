package abs.backend.java.lib.types;


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
}
