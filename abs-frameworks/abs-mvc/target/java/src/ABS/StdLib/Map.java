package ABS.StdLib;
// abslang.abs:335:0: 
public abstract class Map<A extends abs.backend.java.lib.types.ABSValue,B extends abs.backend.java.lib.types.ABSValue> extends abs.backend.java.lib.types.ABSDataType {
    public final boolean isEmptyMap() { return this instanceof Map_EmptyMap; }
    public final Map_EmptyMap<A,B> toEmptyMap() { return (Map_EmptyMap) this; }
    public final boolean isInsertAssoc() { return this instanceof Map_InsertAssoc; }
    public final Map_InsertAssoc<A,B> toInsertAssoc() { return (Map_InsertAssoc) this; }
}
