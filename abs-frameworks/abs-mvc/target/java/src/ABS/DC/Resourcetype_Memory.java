package ABS.DC;
// abslang.abs:622:38: 
public final class Resourcetype_Memory extends Resourcetype {
    public Resourcetype_Memory() {
    }
    protected abs.backend.java.lib.types.ABSValue[] getArgs() { return new abs.backend.java.lib.types.ABSValue[] {  }; }
    public java.lang.String getConstructorName() { return "Memory";} 
    public abs.backend.java.lib.types.ABSBool eq(abs.backend.java.lib.types.ABSValue o) {
        if (! (o instanceof Resourcetype_Memory)) return abs.backend.java.lib.types.ABSBool.FALSE;
        Resourcetype_Memory other = (Resourcetype_Memory) o;
        return abs.backend.java.lib.types.ABSBool.TRUE;
    }
    public boolean match(abs.backend.java.lib.expr.PatternConstructor c, abs.backend.java.lib.expr.PatternBinding b) {
        if (!c.constructorClass.equals(this.getClass())) return false;
        return true;
    }
}
