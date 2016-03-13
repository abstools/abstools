package ABS.Productline;
// abslang.abs:1110:0: 
public abstract class Feature extends abs.backend.java.lib.types.ABSDataType {
    public final boolean isProgram() { return this instanceof Feature_Program; }
    public final Feature_Program toProgram() { return (Feature_Program) this; }
    public final boolean isSource() { return this instanceof Feature_Source; }
    public final Feature_Source toSource() { return (Feature_Source) this; }
    public final boolean isType() { return this instanceof Feature_Type; }
    public final Feature_Type toType() { return (Feature_Type) this; }
    public final boolean isGuidePage() { return this instanceof Feature_GuidePage; }
    public final Feature_GuidePage toGuidePage() { return (Feature_GuidePage) this; }
}
