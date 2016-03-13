package ABS.Meta;
// abslang.abs:1070:0: 
public interface Reconfiguration_i extends abs.backend.java.lib.types.ABSInterface {
    // abslang.abs:1071:4: 
    public  abs.backend.java.lib.runtime.ABSFut<abs.backend.java.lib.types.ABSString> async_getName();
    // abslang.abs:1071:4: 
    public  abs.backend.java.lib.types.ABSString getName();
    // abslang.abs:1072:4: 
    public  abs.backend.java.lib.runtime.ABSFut<ABS.Meta.Product_i> async_getCurrentProduct();
    // abslang.abs:1072:4: 
    public  ABS.Meta.Product_i getCurrentProduct();
    // abslang.abs:1073:4: 
    public  abs.backend.java.lib.runtime.ABSFut<ABS.Meta.Product_i> async_getProduct(abs.backend.java.lib.types.ABSString s);
    // abslang.abs:1073:4: 
    public  ABS.Meta.Product_i getProduct(abs.backend.java.lib.types.ABSString s);
    // abslang.abs:1074:4: 
    public  abs.backend.java.lib.runtime.ABSFut<abs.backend.java.lib.types.ABSUnit> async_setCurrentProduct(ABS.Meta.Product_i p);
    // abslang.abs:1074:4: 
    public  abs.backend.java.lib.types.ABSUnit setCurrentProduct(ABS.Meta.Product_i p);
    // abslang.abs:1075:4: 
    public  abs.backend.java.lib.runtime.ABSFut<ABS.Meta.Product_i> async_getTargetProduct();
    // abslang.abs:1075:4: 
    public  ABS.Meta.Product_i getTargetProduct();
    // abslang.abs:1076:4: 
    public  abs.backend.java.lib.runtime.ABSFut<abs.backend.java.lib.types.ABSUnit> async_setTargetProduct(ABS.Meta.Product_i p);
    // abslang.abs:1076:4: 
    public  abs.backend.java.lib.types.ABSUnit setTargetProduct(ABS.Meta.Product_i p);
    // abslang.abs:1077:4: 
    public  abs.backend.java.lib.runtime.ABSFut<ABS.StdLib.List<ABS.Meta.Delta_i>> async_getDeltas();
    // abslang.abs:1077:4: 
    public  ABS.StdLib.List<ABS.Meta.Delta_i> getDeltas();
    // abslang.abs:1078:4: 
    public  abs.backend.java.lib.runtime.ABSFut<abs.backend.java.lib.types.ABSUnit> async_setDeltas(ABS.StdLib.List<ABS.Meta.Delta_i> deltas);
    // abslang.abs:1078:4: 
    public  abs.backend.java.lib.types.ABSUnit setDeltas(ABS.StdLib.List<ABS.Meta.Delta_i> deltas);
    // abslang.abs:1079:4: 
    public  abs.backend.java.lib.runtime.ABSFut<ABS.Meta.StateUpdate_i> async_getStateUpdate();
    // abslang.abs:1079:4: 
    public  ABS.Meta.StateUpdate_i getStateUpdate();
    // abslang.abs:1080:4: 
    public  abs.backend.java.lib.runtime.ABSFut<abs.backend.java.lib.types.ABSUnit> async_setStateUpdate(ABS.Meta.StateUpdate_i u);
    // abslang.abs:1080:4: 
    public  abs.backend.java.lib.types.ABSUnit setStateUpdate(ABS.Meta.StateUpdate_i u);
}
