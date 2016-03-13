package ABS.DC;
// abslang.abs:623:38: 
public final class Resourcetype_PaymentInterval extends Resourcetype {
    public Resourcetype_PaymentInterval() {
    }
    protected abs.backend.java.lib.types.ABSValue[] getArgs() { return new abs.backend.java.lib.types.ABSValue[] {  }; }
    public java.lang.String getConstructorName() { return "PaymentInterval";} 
    public abs.backend.java.lib.types.ABSBool eq(abs.backend.java.lib.types.ABSValue o) {
        if (! (o instanceof Resourcetype_PaymentInterval)) return abs.backend.java.lib.types.ABSBool.FALSE;
        Resourcetype_PaymentInterval other = (Resourcetype_PaymentInterval) o;
        return abs.backend.java.lib.types.ABSBool.TRUE;
    }
    public boolean match(abs.backend.java.lib.expr.PatternConstructor c, abs.backend.java.lib.expr.PatternBinding b) {
        if (!c.constructorClass.equals(this.getClass())) return false;
        return true;
    }
}
