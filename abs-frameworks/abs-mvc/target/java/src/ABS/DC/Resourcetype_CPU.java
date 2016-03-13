package ABS.DC;
// abslang.abs:622:20: 
public final class Resourcetype_CPU extends Resourcetype {
    public Resourcetype_CPU() {
    }
    protected abs.backend.java.lib.types.ABSValue[] getArgs() { return new abs.backend.java.lib.types.ABSValue[] {  }; }
    public java.lang.String getConstructorName() { return "CPU";} 
    public abs.backend.java.lib.types.ABSBool eq(abs.backend.java.lib.types.ABSValue o) {
        if (! (o instanceof Resourcetype_CPU)) return abs.backend.java.lib.types.ABSBool.FALSE;
        Resourcetype_CPU other = (Resourcetype_CPU) o;
        return abs.backend.java.lib.types.ABSBool.TRUE;
    }
    public boolean match(abs.backend.java.lib.expr.PatternConstructor c, abs.backend.java.lib.expr.PatternBinding b) {
        if (!c.constructorClass.equals(this.getClass())) return false;
        return true;
    }
}
