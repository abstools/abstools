package ABS.StdLib;
// abslang.abs:462:12: 
public final class Time_Time extends Time {
    public final abs.backend.java.lib.types.ABSRational arg0;
    public abs.backend.java.lib.types.ABSRational getArg0() { return arg0; }
    public Time_Time(final abs.backend.java.lib.types.ABSRational arg0) {
        this.arg0 = arg0;
    }
    protected abs.backend.java.lib.types.ABSValue[] getArgs() { return new abs.backend.java.lib.types.ABSValue[] { this.arg0 }; }
    public java.lang.String getConstructorName() { return "Time";} 
    public abs.backend.java.lib.types.ABSBool eq(abs.backend.java.lib.types.ABSValue o) {
        if (! (o instanceof Time_Time)) return abs.backend.java.lib.types.ABSBool.FALSE;
        Time_Time other = (Time_Time) o;
        if (!this.arg0.eq(other.arg0).toBoolean()) return abs.backend.java.lib.types.ABSBool.FALSE;
        return abs.backend.java.lib.types.ABSBool.TRUE;
    }
    public boolean match(abs.backend.java.lib.expr.PatternConstructor c, abs.backend.java.lib.expr.PatternBinding b) {
        if (!c.constructorClass.equals(this.getClass())) return false;
        if (!c.subpattern[0].match(this.arg0, b)) return false;
        return true;
    }
}
