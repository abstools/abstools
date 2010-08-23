package abs.backend.java.lib.expr;

import abs.backend.java.lib.types.ABSDataType;

public class AnyPattern extends Pattern {

	@Override
   public boolean match(ABSDataType dt, PatternBinding binding) {
	   return true;
   }
	

}
