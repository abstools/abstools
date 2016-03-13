package ABS.Meta;
// abslang.abs:1041:0: 
public interface Class_i extends abs.backend.java.lib.types.ABSInterface {
    // abslang.abs:1042:4: 
    public  abs.backend.java.lib.runtime.ABSFut<abs.backend.java.lib.types.ABSString> async_getName();
    // abslang.abs:1042:4: 
    public  abs.backend.java.lib.types.ABSString getName();
    // abslang.abs:1043:4: 
    public  abs.backend.java.lib.runtime.ABSFut<ABS.Meta.Method_i> async_getMethod(abs.backend.java.lib.types.ABSString mName);
    // abslang.abs:1043:4: 
    public  ABS.Meta.Method_i getMethod(abs.backend.java.lib.types.ABSString mName);
    // abslang.abs:1044:4: 
    public  abs.backend.java.lib.runtime.ABSFut<abs.backend.java.lib.types.ABSUnit> async_addMethod(abs.backend.java.lib.types.ABSString mName, ABS.Meta.Method_i m);
    // abslang.abs:1044:4: 
    public  abs.backend.java.lib.types.ABSUnit addMethod(abs.backend.java.lib.types.ABSString mName, ABS.Meta.Method_i m);
    // abslang.abs:1045:4: 
    public  abs.backend.java.lib.runtime.ABSFut<abs.backend.java.lib.types.ABSUnit> async_removeMethod(abs.backend.java.lib.types.ABSString mName);
    // abslang.abs:1045:4: 
    public  abs.backend.java.lib.types.ABSUnit removeMethod(abs.backend.java.lib.types.ABSString mName);
}
