package abs.backend.java.lib.runtime;

import abs.backend.java.observing.GuardView;

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
	
	public String toString() {
	    return "ANDGuard "+left+" AND "+right;
	}

    public ABSGuard getLeftGuard() {
        return left;
    }

    public ABSGuard getRightGuard() {
        return right;
    }
}
