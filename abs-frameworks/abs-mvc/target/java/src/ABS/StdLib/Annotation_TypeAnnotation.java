package ABS.StdLib;
// abslang.abs:520:18: 
public final class Annotation_TypeAnnotation extends Annotation {
    public Annotation_TypeAnnotation() {
    }
    protected abs.backend.java.lib.types.ABSValue[] getArgs() { return new abs.backend.java.lib.types.ABSValue[] {  }; }
    public java.lang.String getConstructorName() { return "TypeAnnotation";} 
    public abs.backend.java.lib.types.ABSBool eq(abs.backend.java.lib.types.ABSValue o) {
        if (! (o instanceof Annotation_TypeAnnotation)) return abs.backend.java.lib.types.ABSBool.FALSE;
        Annotation_TypeAnnotation other = (Annotation_TypeAnnotation) o;
        return abs.backend.java.lib.types.ABSBool.TRUE;
    }
    public boolean match(abs.backend.java.lib.expr.PatternConstructor c, abs.backend.java.lib.expr.PatternBinding b) {
        if (!c.constructorClass.equals(this.getClass())) return false;
        return true;
    }
}
