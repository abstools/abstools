package ABS.StdLib;
// abslang.abs:473:16: 
public final class Duration_Duration extends Duration {
    public final abs.backend.java.lib.types.ABSRational arg0;
    public abs.backend.java.lib.types.ABSRational getArg0() { return arg0; }
    public Duration_Duration(final abs.backend.java.lib.types.ABSRational arg0) {
        this.arg0 = arg0;
    }
    protected abs.backend.java.lib.types.ABSValue[] getArgs() { return new abs.backend.java.lib.types.ABSValue[] { this.arg0 }; }
    public java.lang.String getConstructorName() { return "Duration";} 
    public abs.backend.java.lib.types.ABSBool eq(abs.backend.java.lib.types.ABSValue o) {
        if (! (o instanceof Duration_Duration)) return abs.backend.java.lib.types.ABSBool.FALSE;
        Duration_Duration other = (Duration_Duration) o;
        if (!this.arg0.eq(other.arg0).toBoolean()) return abs.backend.java.lib.types.ABSBool.FALSE;
        return abs.backend.java.lib.types.ABSBool.TRUE;
    }
    public boolean match(abs.backend.java.lib.expr.PatternConstructor c, abs.backend.java.lib.expr.PatternBinding b) {
        if (!c.constructorClass.equals(this.getClass())) return false;
        if (!c.subpattern[0].match(this.arg0, b)) return false;
        return true;
    }
}
