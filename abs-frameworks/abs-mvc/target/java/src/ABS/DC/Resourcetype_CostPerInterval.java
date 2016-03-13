package ABS.DC;
// abslang.abs:623:56: 
public final class Resourcetype_CostPerInterval extends Resourcetype {
    public Resourcetype_CostPerInterval() {
    }
    protected abs.backend.java.lib.types.ABSValue[] getArgs() { return new abs.backend.java.lib.types.ABSValue[] {  }; }
    public java.lang.String getConstructorName() { return "CostPerInterval";} 
    public abs.backend.java.lib.types.ABSBool eq(abs.backend.java.lib.types.ABSValue o) {
        if (! (o instanceof Resourcetype_CostPerInterval)) return abs.backend.java.lib.types.ABSBool.FALSE;
        Resourcetype_CostPerInterval other = (Resourcetype_CostPerInterval) o;
        return abs.backend.java.lib.types.ABSBool.TRUE;
    }
    public boolean match(abs.backend.java.lib.expr.PatternConstructor c, abs.backend.java.lib.expr.PatternBinding b) {
        if (!c.constructorClass.equals(this.getClass())) return false;
        return true;
    }
}
