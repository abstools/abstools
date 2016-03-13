package ABS.StdLib;
// abslang.abs:532:27: 
public final class ClassKindAnnotation_COG extends ClassKindAnnotation {
    public ClassKindAnnotation_COG() {
    }
    protected abs.backend.java.lib.types.ABSValue[] getArgs() { return new abs.backend.java.lib.types.ABSValue[] {  }; }
    public java.lang.String getConstructorName() { return "COG";} 
    public abs.backend.java.lib.types.ABSBool eq(abs.backend.java.lib.types.ABSValue o) {
        if (! (o instanceof ClassKindAnnotation_COG)) return abs.backend.java.lib.types.ABSBool.FALSE;
        ClassKindAnnotation_COG other = (ClassKindAnnotation_COG) o;
        return abs.backend.java.lib.types.ABSBool.TRUE;
    }
    public boolean match(abs.backend.java.lib.expr.PatternConstructor c, abs.backend.java.lib.expr.PatternBinding b) {
        if (!c.constructorClass.equals(this.getClass())) return false;
        return true;
    }
}
