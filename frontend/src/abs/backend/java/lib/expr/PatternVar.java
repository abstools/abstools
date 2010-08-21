package abs.backend.java.lib.expr;

import abs.backend.java.lib.types.ABSDataType;

public class PatternVar extends Pattern {

	@Override
   public boolean matches(ABSDataType dt) {
	   return true;
   }
	
}
