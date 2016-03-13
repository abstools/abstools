package ABS.StdLib;
// abslang.abs:122:35: 
public final class Either_Right<A extends abs.backend.java.lib.types.ABSValue,B extends abs.backend.java.lib.types.ABSValue> extends Either<A,B> {
    public final B arg0;
    public B getArg0() { return arg0; }
    public Either_Right(final B arg0) {
        this.arg0 = arg0;
    }
    protected abs.backend.java.lib.types.ABSValue[] getArgs() { return new abs.backend.java.lib.types.ABSValue[] { this.arg0 }; }
    public java.lang.String getConstructorName() { return "Right";} 
    public abs.backend.java.lib.types.ABSBool eq(abs.backend.java.lib.types.ABSValue o) {
        if (! (o instanceof Either_Right)) return abs.backend.java.lib.types.ABSBool.FALSE;
        Either_Right other = (Either_Right) o;
        if (!this.arg0.eq(other.arg0).toBoolean()) return abs.backend.java.lib.types.ABSBool.FALSE;
        return abs.backend.java.lib.types.ABSBool.TRUE;
    }
    public boolean match(abs.backend.java.lib.expr.PatternConstructor c, abs.backend.java.lib.expr.PatternBinding b) {
        if (!c.constructorClass.equals(this.getClass())) return false;
        if (!c.subpattern[0].match(this.arg0, b)) return false;
        return true;
    }
}
