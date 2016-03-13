package ABS.StdLib;
// abslang.abs:520:0: 
public abstract class Annotation extends abs.backend.java.lib.types.ABSDataType {
    public final boolean isTypeAnnotation() { return this instanceof Annotation_TypeAnnotation; }
    public final Annotation_TypeAnnotation toTypeAnnotation() { return (Annotation_TypeAnnotation) this; }
}
