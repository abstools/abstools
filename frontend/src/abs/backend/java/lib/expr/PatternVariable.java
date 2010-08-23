package abs.backend.java.lib.expr;

import abs.backend.java.lib.types.ABSDataType;

public class PatternVariable extends Pattern {
   private final String varName;
   public PatternVariable(String name) {
       this.varName = name;
   }
    
	@Override
   public boolean match(ABSDataType dt, PatternBinding b) {
	   b.addBinding(dt);
	   return true;
   }
	
}
