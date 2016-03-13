package ABS.StdLib;
// abslang.abs:543:0: 
public abstract class AtomicityAnnotation extends abs.backend.java.lib.types.ABSDataType {
    public final boolean isAtomic() { return this instanceof AtomicityAnnotation_Atomic; }
    public final AtomicityAnnotation_Atomic toAtomic() { return (AtomicityAnnotation_Atomic) this; }
}
