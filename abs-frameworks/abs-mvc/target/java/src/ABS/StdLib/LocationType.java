package ABS.StdLib;
// abslang.abs:522:0: 
public abstract class LocationType extends abs.backend.java.lib.types.ABSDataType {
    public final boolean isFar() { return this instanceof LocationType_Far; }
    public final LocationType_Far toFar() { return (LocationType_Far) this; }
    public final boolean isNear() { return this instanceof LocationType_Near; }
    public final LocationType_Near toNear() { return (LocationType_Near) this; }
    public final boolean isSomewhere() { return this instanceof LocationType_Somewhere; }
    public final LocationType_Somewhere toSomewhere() { return (LocationType_Somewhere) this; }
    public final boolean isInfer() { return this instanceof LocationType_Infer; }
    public final LocationType_Infer toInfer() { return (LocationType_Infer) this; }
}
