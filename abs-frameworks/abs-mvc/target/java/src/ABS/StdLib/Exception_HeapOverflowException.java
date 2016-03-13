package ABS.StdLib;
// abslang.abs:565:0: 
public final class Exception_HeapOverflowException extends Exception {
    public Exception_HeapOverflowException() {
    }
    protected abs.backend.java.lib.types.ABSValue[] getArgs() { return new abs.backend.java.lib.types.ABSValue[] {  }; }
    public java.lang.String getConstructorName() { return "HeapOverflowException";} 
    public abs.backend.java.lib.types.ABSBool eq(abs.backend.java.lib.types.ABSValue o) {
        if (! (o instanceof Exception_HeapOverflowException)) return abs.backend.java.lib.types.ABSBool.FALSE;
        Exception_HeapOverflowException other = (Exception_HeapOverflowException) o;
        return abs.backend.java.lib.types.ABSBool.TRUE;
    }
    public boolean match(abs.backend.java.lib.expr.PatternConstructor c, abs.backend.java.lib.expr.PatternBinding b) {
        if (!c.constructorClass.equals(this.getClass())) return false;
        return true;
    }
}
