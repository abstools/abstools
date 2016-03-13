package ABS.DC;
// abslang.abs:622:26: 
public final class Resourcetype_Bandwidth extends Resourcetype {
    public Resourcetype_Bandwidth() {
    }
    protected abs.backend.java.lib.types.ABSValue[] getArgs() { return new abs.backend.java.lib.types.ABSValue[] {  }; }
    public java.lang.String getConstructorName() { return "Bandwidth";} 
    public abs.backend.java.lib.types.ABSBool eq(abs.backend.java.lib.types.ABSValue o) {
        if (! (o instanceof Resourcetype_Bandwidth)) return abs.backend.java.lib.types.ABSBool.FALSE;
        Resourcetype_Bandwidth other = (Resourcetype_Bandwidth) o;
        return abs.backend.java.lib.types.ABSBool.TRUE;
    }
    public boolean match(abs.backend.java.lib.expr.PatternConstructor c, abs.backend.java.lib.expr.PatternBinding b) {
        if (!c.constructorClass.equals(this.getClass())) return false;
        return true;
    }
}
