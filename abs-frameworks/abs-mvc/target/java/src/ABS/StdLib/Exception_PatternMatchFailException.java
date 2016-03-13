package ABS.StdLib;
// abslang.abs:562:0: 
public final class Exception_PatternMatchFailException extends Exception {
    public Exception_PatternMatchFailException() {
    }
    protected abs.backend.java.lib.types.ABSValue[] getArgs() { return new abs.backend.java.lib.types.ABSValue[] {  }; }
    public java.lang.String getConstructorName() { return "PatternMatchFailException";} 
    public abs.backend.java.lib.types.ABSBool eq(abs.backend.java.lib.types.ABSValue o) {
        if (! (o instanceof Exception_PatternMatchFailException)) return abs.backend.java.lib.types.ABSBool.FALSE;
        Exception_PatternMatchFailException other = (Exception_PatternMatchFailException) o;
        return abs.backend.java.lib.types.ABSBool.TRUE;
    }
    public boolean match(abs.backend.java.lib.expr.PatternConstructor c, abs.backend.java.lib.expr.PatternBinding b) {
        if (!c.constructorClass.equals(this.getClass())) return false;
        return true;
    }
}
