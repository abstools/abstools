package ABS.StdLib;
// abslang.abs:140:14: 
public final class Set_EmptySet<A extends abs.backend.java.lib.types.ABSValue> extends Set<A> {
    public Set_EmptySet() {
    }
    protected abs.backend.java.lib.types.ABSValue[] getArgs() { return new abs.backend.java.lib.types.ABSValue[] {  }; }
    public java.lang.String getConstructorName() { return "EmptySet";} 
    public abs.backend.java.lib.types.ABSBool eq(abs.backend.java.lib.types.ABSValue o) {
        if (! (o instanceof Set_EmptySet)) return abs.backend.java.lib.types.ABSBool.FALSE;
        Set_EmptySet other = (Set_EmptySet) o;
        return abs.backend.java.lib.types.ABSBool.TRUE;
    }
    public boolean match(abs.backend.java.lib.expr.PatternConstructor c, abs.backend.java.lib.expr.PatternBinding b) {
        if (!c.constructorClass.equals(this.getClass())) return false;
        return true;
    }
}
