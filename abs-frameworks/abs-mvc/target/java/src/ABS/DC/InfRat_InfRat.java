package ABS.DC;
// abslang.abs:625:14: 
public final class InfRat_InfRat extends InfRat {
    public InfRat_InfRat() {
    }
    protected abs.backend.java.lib.types.ABSValue[] getArgs() { return new abs.backend.java.lib.types.ABSValue[] {  }; }
    public java.lang.String getConstructorName() { return "InfRat";} 
    public abs.backend.java.lib.types.ABSBool eq(abs.backend.java.lib.types.ABSValue o) {
        if (! (o instanceof InfRat_InfRat)) return abs.backend.java.lib.types.ABSBool.FALSE;
        InfRat_InfRat other = (InfRat_InfRat) o;
        return abs.backend.java.lib.types.ABSBool.TRUE;
    }
    public boolean match(abs.backend.java.lib.expr.PatternConstructor c, abs.backend.java.lib.expr.PatternBinding b) {
        if (!c.constructorClass.equals(this.getClass())) return false;
        return true;
    }
}
