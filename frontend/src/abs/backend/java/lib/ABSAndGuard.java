package abs.backend.java.lib;

public class ABSAndGuard extends ABSGuard {
	public final ABSGuard left;
	public final ABSGuard right;
	public ABSAndGuard(ABSGuard l, ABSGuard r) {
		left = l;
		right = r;
	}

	@Override
   public boolean isTrue() {
	   return left.isTrue() && right.isTrue();
   }

	@Override
   public void await() {
	   left.await(); 
	   right.await();
   }
	
	
}
