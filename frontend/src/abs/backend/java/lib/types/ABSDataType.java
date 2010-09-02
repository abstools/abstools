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

   private static final ABSValue[] NO_ARGS = new ABSValue[0];
   protected ABSValue[] getArgs() {
       return NO_ARGS;
   }
   
   public boolean isBuiltIn() {
   	return false;
   }
   
   public String getConstructorName() {
       return this.getClass().getSimpleName();
   }
   
   public String toString() {
       StringBuilder sb = new StringBuilder();
       toStringBuilder(sb);
       return sb.toString();
   }
   
   private void toStringBuilder(StringBuilder sb) {
       sb.append(getConstructorName());
       ABSValue[] args = getArgs();
       if (args.length > 0) {
           sb.append('(');
           int i = 0;
           for (ABSValue v : args) {
               if (i > 0) {
                   sb.append(',');
               }
               
               sb.append(v.toString());
               i++;
           }
           sb.append(')');
       }
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
