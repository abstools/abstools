package abs.backend.java.lib.types;

import abs.backend.java.lib.expr.PatternBinding;
import abs.backend.java.lib.expr.PatternConstructor;


public abstract class ABSDataType implements ABSValue {

    public ABSBool eq(ABSValue other) {
   	if (other == null || other.getClass() != this.getClass())
   		return ABSBool.FALSE;
   	return ABSBool.TRUE;
   }

   public ABSBool notEq(ABSValue other) {
   	return this.eq(other).negate();   	
   }

   public abstract boolean match(PatternConstructor p, PatternBinding b);

   public boolean isBuiltIn() {
   	return false;
   }
   
   public String getConstructorName() {
       return this.getClass().getSimpleName();
   }
   
   public String toString() {
       return getConstructorName();
   }
   
   @Override
   public boolean isDataType() {
      return true;
   }
   
   @Override
   public boolean isReference() {
       return false;
   }
   
}
