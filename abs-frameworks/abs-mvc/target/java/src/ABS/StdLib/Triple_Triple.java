package ABS.StdLib;
// abslang.abs:132:23: 
public final class Triple_Triple<A extends abs.backend.java.lib.types.ABSValue,B extends abs.backend.java.lib.types.ABSValue,C extends abs.backend.java.lib.types.ABSValue> extends Triple<A,B,C> {
    public final A arg0;
    public A getArg0() { return arg0; }
    public final B arg1;
    public B getArg1() { return arg1; }
    public final C arg2;
    public C getArg2() { return arg2; }
    public Triple_Triple(final A arg0, final B arg1, final C arg2) {
        this.arg0 = arg0;
        this.arg1 = arg1;
        this.arg2 = arg2;
    }
    protected abs.backend.java.lib.types.ABSValue[] getArgs() { return new abs.backend.java.lib.types.ABSValue[] { this.arg0, this.arg1, this.arg2 }; }
    public java.lang.String getConstructorName() { return "Triple";} 
    public abs.backend.java.lib.types.ABSBool eq(abs.backend.java.lib.types.ABSValue o) {
        if (! (o instanceof Triple_Triple)) return abs.backend.java.lib.types.ABSBool.FALSE;
        Triple_Triple other = (Triple_Triple) o;
        if (!this.arg0.eq(other.arg0).toBoolean()) return abs.backend.java.lib.types.ABSBool.FALSE;
        if (!this.arg1.eq(other.arg1).toBoolean()) return abs.backend.java.lib.types.ABSBool.FALSE;
        if (!this.arg2.eq(other.arg2).toBoolean()) return abs.backend.java.lib.types.ABSBool.FALSE;
        return abs.backend.java.lib.types.ABSBool.TRUE;
    }
    public boolean match(abs.backend.java.lib.expr.PatternConstructor c, abs.backend.java.lib.expr.PatternBinding b) {
        if (!c.constructorClass.equals(this.getClass())) return false;
        if (!c.subpattern[0].match(this.arg0, b)) return false;
        if (!c.subpattern[1].match(this.arg1, b)) return false;
        if (!c.subpattern[2].match(this.arg2, b)) return false;
        return true;
    }
}
