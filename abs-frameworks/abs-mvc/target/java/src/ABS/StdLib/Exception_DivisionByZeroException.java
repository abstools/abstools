package ABS.StdLib;
// abslang.abs:560:0: 
public final class Exception_DivisionByZeroException extends Exception {
    public Exception_DivisionByZeroException() {
    }
    protected abs.backend.java.lib.types.ABSValue[] getArgs() { return new abs.backend.java.lib.types.ABSValue[] {  }; }
    public java.lang.String getConstructorName() { return "DivisionByZeroException";} 
    public abs.backend.java.lib.types.ABSBool eq(abs.backend.java.lib.types.ABSValue o) {
        if (! (o instanceof Exception_DivisionByZeroException)) return abs.backend.java.lib.types.ABSBool.FALSE;
        Exception_DivisionByZeroException other = (Exception_DivisionByZeroException) o;
        return abs.backend.java.lib.types.ABSBool.TRUE;
    }
    public boolean match(abs.backend.java.lib.expr.PatternConstructor c, abs.backend.java.lib.expr.PatternBinding b) {
        if (!c.constructorClass.equals(this.getClass())) return false;
        return true;
    }
}
