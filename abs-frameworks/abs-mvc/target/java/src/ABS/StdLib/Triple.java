package ABS.StdLib;
// abslang.abs:132:0: 
public abstract class Triple<A extends abs.backend.java.lib.types.ABSValue,B extends abs.backend.java.lib.types.ABSValue,C extends abs.backend.java.lib.types.ABSValue> extends abs.backend.java.lib.types.ABSDataType {
    public final boolean isTriple() { return this instanceof Triple_Triple; }
    public final Triple_Triple<A,B,C> toTriple() { return (Triple_Triple) this; }
}
