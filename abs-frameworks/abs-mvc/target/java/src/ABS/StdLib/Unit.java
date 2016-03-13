package ABS.StdLib;
// abslang.abs:64:0: 
public abstract class Unit extends abs.backend.java.lib.types.ABSDataType {
    public final boolean isUnit() { return this instanceof Unit_Unit; }
    public final Unit_Unit toUnit() { return (Unit_Unit) this; }
}
