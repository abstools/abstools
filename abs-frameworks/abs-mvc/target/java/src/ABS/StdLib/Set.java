package ABS.StdLib;
// abslang.abs:140:0: 
public abstract class Set<A extends abs.backend.java.lib.types.ABSValue> extends abs.backend.java.lib.types.ABSDataType {
    public final boolean isEmptySet() { return this instanceof Set_EmptySet; }
    public final Set_EmptySet<A> toEmptySet() { return (Set_EmptySet) this; }
    public final boolean isInsert() { return this instanceof Set_Insert; }
    public final Set_Insert<A> toInsert() { return (Set_Insert) this; }
}
