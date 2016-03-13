package ABS.StdLib;
// abslang.abs:561:0: 
public final class Exception_AssertionFailException extends Exception {
    public Exception_AssertionFailException() {
    }
    protected abs.backend.java.lib.types.ABSValue[] getArgs() { return new abs.backend.java.lib.types.ABSValue[] {  }; }
    public java.lang.String getConstructorName() { return "AssertionFailException";} 
    public abs.backend.java.lib.types.ABSBool eq(abs.backend.java.lib.types.ABSValue o) {
        if (! (o instanceof Exception_AssertionFailException)) return abs.backend.java.lib.types.ABSBool.FALSE;
        Exception_AssertionFailException other = (Exception_AssertionFailException) o;
        return abs.backend.java.lib.types.ABSBool.TRUE;
    }
    public boolean match(abs.backend.java.lib.expr.PatternConstructor c, abs.backend.java.lib.expr.PatternBinding b) {
        if (!c.constructorClass.equals(this.getClass())) return false;
        return true;
    }
}
