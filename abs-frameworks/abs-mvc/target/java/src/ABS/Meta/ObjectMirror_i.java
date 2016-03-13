package ABS.Meta;
// abslang.abs:1018:0: 
public interface ObjectMirror_i extends abs.backend.java.lib.types.ABSInterface {
    // abslang.abs:1019:4: 
    public  abs.backend.java.lib.runtime.ABSFut<abs.backend.java.lib.types.ABSString> async_getClassName__();
    // abslang.abs:1019:4: 
    public  abs.backend.java.lib.types.ABSString getClassName__();
    // abslang.abs:1020:4: 
    public  abs.backend.java.lib.runtime.ABSFut<ABS.Meta.Class_i> async_getClass__();
    // abslang.abs:1020:4: 
    public  ABS.Meta.Class_i getClass__();
    // abslang.abs:1021:4: 
    public  abs.backend.java.lib.runtime.ABSFut<abs.backend.java.lib.types.ABSUnit> async_setClass(ABS.Meta.Class_i c);
    // abslang.abs:1021:4: 
    public  abs.backend.java.lib.types.ABSUnit setClass(ABS.Meta.Class_i c);
    // abslang.abs:1023:4: 
    public  abs.backend.java.lib.runtime.ABSFut<ABS.Meta.Object_i> async_getFieldValue();
    // abslang.abs:1023:4: 
    public  ABS.Meta.Object_i getFieldValue();
    // abslang.abs:1024:4: 
    public  abs.backend.java.lib.runtime.ABSFut<abs.backend.java.lib.types.ABSUnit> async_setFieldValue(ABS.Meta.Object_i val);
    // abslang.abs:1024:4: 
    public  abs.backend.java.lib.types.ABSUnit setFieldValue(ABS.Meta.Object_i val);
    // abslang.abs:1025:4: 
    public  abs.backend.java.lib.runtime.ABSFut<ABS.Meta.Cog_i> async_getCog();
    // abslang.abs:1025:4: 
    public  ABS.Meta.Cog_i getCog();
    // abslang.abs:1026:4: 
    public  abs.backend.java.lib.runtime.ABSFut<abs.backend.java.lib.types.ABSUnit> async_setCog(ABS.Meta.Cog_i c);
    // abslang.abs:1026:4: 
    public  abs.backend.java.lib.types.ABSUnit setCog(ABS.Meta.Cog_i c);
    // abslang.abs:1028:4: 
    public  abs.backend.java.lib.runtime.ABSFut<abs.backend.java.lib.types.ABSBool> async_respondsTo(abs.backend.java.lib.types.ABSString name);
    // abslang.abs:1028:4: 
    public  abs.backend.java.lib.types.ABSBool respondsTo(abs.backend.java.lib.types.ABSString name);
}
