package ABS.StdLib;
// abslang.abs:532:33: 
public final class ClassKindAnnotation_Plain extends ClassKindAnnotation {
    public ClassKindAnnotation_Plain() {
    }
    protected abs.backend.java.lib.types.ABSValue[] getArgs() { return new abs.backend.java.lib.types.ABSValue[] {  }; }
    public java.lang.String getConstructorName() { return "Plain";} 
    public abs.backend.java.lib.types.ABSBool eq(abs.backend.java.lib.types.ABSValue o) {
        if (! (o instanceof ClassKindAnnotation_Plain)) return abs.backend.java.lib.types.ABSBool.FALSE;
        ClassKindAnnotation_Plain other = (ClassKindAnnotation_Plain) o;
        return abs.backend.java.lib.types.ABSBool.TRUE;
    }
    public boolean match(abs.backend.java.lib.expr.PatternConstructor c, abs.backend.java.lib.expr.PatternBinding b) {
        if (!c.constructorClass.equals(this.getClass())) return false;
        return true;
    }
}
