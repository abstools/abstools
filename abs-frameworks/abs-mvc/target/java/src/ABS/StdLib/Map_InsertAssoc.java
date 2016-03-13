package ABS.StdLib;
// abslang.abs:335:28: 
public final class Map_InsertAssoc<A extends abs.backend.java.lib.types.ABSValue,B extends abs.backend.java.lib.types.ABSValue> extends Map<A,B> {
    public final ABS.StdLib.Pair<A,B> arg0;
    public ABS.StdLib.Pair<A,B> getArg0() { return arg0; }
    public final ABS.StdLib.Map<A,B> arg1;
    public ABS.StdLib.Map<A,B> getArg1() { return arg1; }
    public Map_InsertAssoc(final ABS.StdLib.Pair<A,B> arg0, final ABS.StdLib.Map<A,B> arg1) {
        this.arg0 = arg0;
        this.arg1 = arg1;
    }
    protected abs.backend.java.lib.types.ABSValue[] getArgs() { return new abs.backend.java.lib.types.ABSValue[] { this.arg0, this.arg1 }; }
    public java.lang.String getConstructorName() { return "InsertAssoc";} 
    public abs.backend.java.lib.types.ABSBool eq(abs.backend.java.lib.types.ABSValue o) {
        if (! (o instanceof Map_InsertAssoc)) return abs.backend.java.lib.types.ABSBool.FALSE;
        Map_InsertAssoc other = (Map_InsertAssoc) o;
        if (!this.arg0.eq(other.arg0).toBoolean()) return abs.backend.java.lib.types.ABSBool.FALSE;
        if (!this.arg1.eq(other.arg1).toBoolean()) return abs.backend.java.lib.types.ABSBool.FALSE;
        return abs.backend.java.lib.types.ABSBool.TRUE;
    }
    public boolean match(abs.backend.java.lib.expr.PatternConstructor c, abs.backend.java.lib.expr.PatternBinding b) {
        if (!c.constructorClass.equals(this.getClass())) return false;
        if (!c.subpattern[0].match(this.arg0, b)) return false;
        if (!c.subpattern[1].match(this.arg1, b)) return false;
        return true;
    }
}
