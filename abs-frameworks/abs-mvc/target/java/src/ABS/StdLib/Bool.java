package ABS.StdLib;
// abslang.abs:68:0: 
public abstract class Bool extends abs.backend.java.lib.types.ABSDataType {
    public final boolean isTrue() { return this instanceof Bool_True; }
    public final Bool_True toTrue() { return (Bool_True) this; }
    public final boolean isFalse() { return this instanceof Bool_False; }
    public final Bool_False toFalse() { return (Bool_False) this; }
}
