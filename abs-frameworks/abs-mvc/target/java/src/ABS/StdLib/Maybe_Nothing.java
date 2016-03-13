package ABS.StdLib;
// abslang.abs:117:16: 
public final class Maybe_Nothing<A extends abs.backend.java.lib.types.ABSValue> extends Maybe<A> {
    public Maybe_Nothing() {
    }
    protected abs.backend.java.lib.types.ABSValue[] getArgs() { return new abs.backend.java.lib.types.ABSValue[] {  }; }
    public java.lang.String getConstructorName() { return "Nothing";} 
    public abs.backend.java.lib.types.ABSBool eq(abs.backend.java.lib.types.ABSValue o) {
        if (! (o instanceof Maybe_Nothing)) return abs.backend.java.lib.types.ABSBool.FALSE;
        Maybe_Nothing other = (Maybe_Nothing) o;
        return abs.backend.java.lib.types.ABSBool.TRUE;
    }
    public boolean match(abs.backend.java.lib.expr.PatternConstructor c, abs.backend.java.lib.expr.PatternBinding b) {
        if (!c.constructorClass.equals(this.getClass())) return false;
        return true;
    }
}
