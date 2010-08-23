package abs.backend.java.lib.types;

import abs.backend.java.lib.expr.PatternBinding;
import abs.backend.java.lib.expr.PatternConstructor;


public abstract class ABSDataType implements ABSType {
   public ABSBool eq(ABSDataType other) {
   	if (other == null || other.getClass() != this.getClass())
   		return ABSBool.FALSE;
   	return ABSBool.TRUE;
   }

   public ABSBool notEq(ABSDataType other) {
   	return this.eq(other).negate();   	
   }

   public abstract boolean match(PatternConstructor p, PatternBinding b);

   public boolean isBuiltIn() {
   	return false;
   }
}
