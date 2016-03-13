package ABS.StdLib;
// abslang.abs:563:0: 
public final class Exception_NullPointerException extends Exception {
    public Exception_NullPointerException() {
    }
    protected abs.backend.java.lib.types.ABSValue[] getArgs() { return new abs.backend.java.lib.types.ABSValue[] {  }; }
    public java.lang.String getConstructorName() { return "NullPointerException";} 
    public abs.backend.java.lib.types.ABSBool eq(abs.backend.java.lib.types.ABSValue o) {
        if (! (o instanceof Exception_NullPointerException)) return abs.backend.java.lib.types.ABSBool.FALSE;
        Exception_NullPointerException other = (Exception_NullPointerException) o;
        return abs.backend.java.lib.types.ABSBool.TRUE;
    }
    public boolean match(abs.backend.java.lib.expr.PatternConstructor c, abs.backend.java.lib.expr.PatternBinding b) {
        if (!c.constructorClass.equals(this.getClass())) return false;
        return true;
    }
}
