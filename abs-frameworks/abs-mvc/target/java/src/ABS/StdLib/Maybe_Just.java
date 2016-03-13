package ABS.StdLib;
// abslang.abs:117:26: 
public final class Maybe_Just<A extends abs.backend.java.lib.types.ABSValue> extends Maybe<A> {
    public final A arg0;
    public A getArg0() { return arg0; }
    public Maybe_Just(final A arg0) {
        this.arg0 = arg0;
    }
    protected abs.backend.java.lib.types.ABSValue[] getArgs() { return new abs.backend.java.lib.types.ABSValue[] { this.arg0 }; }
    public java.lang.String getConstructorName() { return "Just";} 
    public abs.backend.java.lib.types.ABSBool eq(abs.backend.java.lib.types.ABSValue o) {
        if (! (o instanceof Maybe_Just)) return abs.backend.java.lib.types.ABSBool.FALSE;
        Maybe_Just other = (Maybe_Just) o;
        if (!this.arg0.eq(other.arg0).toBoolean()) return abs.backend.java.lib.types.ABSBool.FALSE;
        return abs.backend.java.lib.types.ABSBool.TRUE;
    }
    public boolean match(abs.backend.java.lib.expr.PatternConstructor c, abs.backend.java.lib.expr.PatternBinding b) {
        if (!c.constructorClass.equals(this.getClass())) return false;
        if (!c.subpattern[0].match(this.arg0, b)) return false;
        return true;
    }
}
