package ABS.StdLib;
// abslang.abs:130:0: 
public abstract class Pair<A extends abs.backend.java.lib.types.ABSValue,B extends abs.backend.java.lib.types.ABSValue> extends abs.backend.java.lib.types.ABSDataType {
    public final boolean isPair() { return this instanceof Pair_Pair; }
    public final Pair_Pair<A,B> toPair() { return (Pair_Pair) this; }
}
