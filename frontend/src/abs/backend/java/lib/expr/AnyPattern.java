package abs.backend.java.lib.expr;

import abs.backend.java.lib.types.ABSDataType;

public class AnyPattern extends Pattern {

	@Override
   public boolean matches(ABSDataType dt) {
	   return true;
   }
	

}
