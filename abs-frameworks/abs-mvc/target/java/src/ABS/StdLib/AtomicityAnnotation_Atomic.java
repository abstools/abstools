package ABS.StdLib;
// abslang.abs:543:27: 
public final class AtomicityAnnotation_Atomic extends AtomicityAnnotation {
    public AtomicityAnnotation_Atomic() {
    }
    protected abs.backend.java.lib.types.ABSValue[] getArgs() { return new abs.backend.java.lib.types.ABSValue[] {  }; }
    public java.lang.String getConstructorName() { return "Atomic";} 
    public abs.backend.java.lib.types.ABSBool eq(abs.backend.java.lib.types.ABSValue o) {
        if (! (o instanceof AtomicityAnnotation_Atomic)) return abs.backend.java.lib.types.ABSBool.FALSE;
        AtomicityAnnotation_Atomic other = (AtomicityAnnotation_Atomic) o;
        return abs.backend.java.lib.types.ABSBool.TRUE;
    }
    public boolean match(abs.backend.java.lib.expr.PatternConstructor c, abs.backend.java.lib.expr.PatternBinding b) {
        if (!c.constructorClass.equals(this.getClass())) return false;
        return true;
    }
}
