package ABS.StdLib;
// abslang.abs:564:0: 
public final class Exception_StackOverflowEcxeption extends Exception {
    public Exception_StackOverflowEcxeption() {
    }
    protected abs.backend.java.lib.types.ABSValue[] getArgs() { return new abs.backend.java.lib.types.ABSValue[] {  }; }
    public java.lang.String getConstructorName() { return "StackOverflowEcxeption";} 
    public abs.backend.java.lib.types.ABSBool eq(abs.backend.java.lib.types.ABSValue o) {
        if (! (o instanceof Exception_StackOverflowEcxeption)) return abs.backend.java.lib.types.ABSBool.FALSE;
        Exception_StackOverflowEcxeption other = (Exception_StackOverflowEcxeption) o;
        return abs.backend.java.lib.types.ABSBool.TRUE;
    }
    public boolean match(abs.backend.java.lib.expr.PatternConstructor c, abs.backend.java.lib.expr.PatternBinding b) {
        if (!c.constructorClass.equals(this.getClass())) return false;
        return true;
    }
}
