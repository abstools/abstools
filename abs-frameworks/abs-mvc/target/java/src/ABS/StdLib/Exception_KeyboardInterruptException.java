package ABS.StdLib;
// abslang.abs:566:0: 
public final class Exception_KeyboardInterruptException extends Exception {
    public Exception_KeyboardInterruptException() {
    }
    protected abs.backend.java.lib.types.ABSValue[] getArgs() { return new abs.backend.java.lib.types.ABSValue[] {  }; }
    public java.lang.String getConstructorName() { return "KeyboardInterruptException";} 
    public abs.backend.java.lib.types.ABSBool eq(abs.backend.java.lib.types.ABSValue o) {
        if (! (o instanceof Exception_KeyboardInterruptException)) return abs.backend.java.lib.types.ABSBool.FALSE;
        Exception_KeyboardInterruptException other = (Exception_KeyboardInterruptException) o;
        return abs.backend.java.lib.types.ABSBool.TRUE;
    }
    public boolean match(abs.backend.java.lib.expr.PatternConstructor c, abs.backend.java.lib.expr.PatternBinding b) {
        if (!c.constructorClass.equals(this.getClass())) return false;
        return true;
    }
}
