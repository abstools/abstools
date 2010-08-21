package abs.backend.java.lib.types;

import abs.backend.java.lib.expr.Pattern;
import abs.backend.java.lib.expr.PatternBinding;


public abstract class ABSDataType implements ABSType {
   public ABSBool eq(ABSDataType other) {
   	if (other == null || other.getClass() != this.getClass())
   		return ABSBool.FALSE;
   	return ABSBool.TRUE;
   }

   public ABSBool notEq(ABSDataType other) {
   	return this.eq(other).negate();   	
   }

   public PatternBinding match(Pattern p) {
   	PatternBinding b = new PatternBinding();
   	boolean matched = match(p,b);
   	if (matched)
   		return b;
   	else
   		return null;
   }

	public boolean match(Pattern p, PatternBinding b) {
		return true;
	}

}
