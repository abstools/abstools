package ABS.StdLib;
// abslang.abs:532:0: 
public abstract class ClassKindAnnotation extends abs.backend.java.lib.types.ABSDataType {
    public final boolean isCOG() { return this instanceof ClassKindAnnotation_COG; }
    public final ClassKindAnnotation_COG toCOG() { return (ClassKindAnnotation_COG) this; }
    public final boolean isPlain() { return this instanceof ClassKindAnnotation_Plain; }
    public final ClassKindAnnotation_Plain toPlain() { return (ClassKindAnnotation_Plain) this; }
}
