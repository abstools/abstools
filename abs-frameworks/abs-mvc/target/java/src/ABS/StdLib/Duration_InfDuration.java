package ABS.StdLib;
// abslang.abs:473:46: 
public final class Duration_InfDuration extends Duration {
    public Duration_InfDuration() {
    }
    protected abs.backend.java.lib.types.ABSValue[] getArgs() { return new abs.backend.java.lib.types.ABSValue[] {  }; }
    public java.lang.String getConstructorName() { return "InfDuration";} 
    public abs.backend.java.lib.types.ABSBool eq(abs.backend.java.lib.types.ABSValue o) {
        if (! (o instanceof Duration_InfDuration)) return abs.backend.java.lib.types.ABSBool.FALSE;
        Duration_InfDuration other = (Duration_InfDuration) o;
        return abs.backend.java.lib.types.ABSBool.TRUE;
    }
    public boolean match(abs.backend.java.lib.expr.PatternConstructor c, abs.backend.java.lib.expr.PatternBinding b) {
        if (!c.constructorClass.equals(this.getClass())) return false;
        return true;
    }
}
