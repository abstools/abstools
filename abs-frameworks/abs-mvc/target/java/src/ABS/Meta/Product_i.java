package ABS.Meta;
// abslang.abs:1062:0: 
public interface Product_i extends abs.backend.java.lib.types.ABSInterface {
    // abslang.abs:1063:4: 
    public  abs.backend.java.lib.runtime.ABSFut<abs.backend.java.lib.types.ABSString> async_getName();
    // abslang.abs:1063:4: 
    public  abs.backend.java.lib.types.ABSString getName();
    // abslang.abs:1064:4: 
    public  abs.backend.java.lib.runtime.ABSFut<ABS.StdLib.Set<abs.backend.java.lib.types.ABSString>> async_getFeatures();
    // abslang.abs:1064:4: 
    public  ABS.StdLib.Set<abs.backend.java.lib.types.ABSString> getFeatures();
    // abslang.abs:1065:4: 
    public  abs.backend.java.lib.runtime.ABSFut<ABS.StdLib.Set<ABS.Meta.Product_i>> async_getConfigurableProducts();
    // abslang.abs:1065:4: 
    public  ABS.StdLib.Set<ABS.Meta.Product_i> getConfigurableProducts();
    // abslang.abs:1066:4: 
    public  abs.backend.java.lib.runtime.ABSFut<ABS.Meta.Reconfiguration_i> async_getReconfiguration(ABS.Meta.Product_i p);
    // abslang.abs:1066:4: 
    public  ABS.Meta.Reconfiguration_i getReconfiguration(ABS.Meta.Product_i p);
    // abslang.abs:1067:4: 
    public  abs.backend.java.lib.runtime.ABSFut<abs.backend.java.lib.types.ABSUnit> async_reconfigure(ABS.Meta.Product_i p);
    // abslang.abs:1067:4: 
    public  abs.backend.java.lib.types.ABSUnit reconfigure(ABS.Meta.Product_i p);
}
