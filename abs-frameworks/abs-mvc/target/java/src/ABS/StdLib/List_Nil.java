package ABS.StdLib;
// abslang.abs:263:15: 
public final class List_Nil<A extends abs.backend.java.lib.types.ABSValue> extends List<A> {
    public List_Nil() {
    }
    protected abs.backend.java.lib.types.ABSValue[] getArgs() { return new abs.backend.java.lib.types.ABSValue[] {  }; }
    public java.lang.String getConstructorName() { return "Nil";} 
    public abs.backend.java.lib.types.ABSBool eq(abs.backend.java.lib.types.ABSValue o) {
        if (! (o instanceof List_Nil)) return abs.backend.java.lib.types.ABSBool.FALSE;
        List_Nil other = (List_Nil) o;
        return abs.backend.java.lib.types.ABSBool.TRUE;
    }
    public boolean match(abs.backend.java.lib.expr.PatternConstructor c, abs.backend.java.lib.expr.PatternBinding b) {
        if (!c.constructorClass.equals(this.getClass())) return false;
        return true;
    }
}
