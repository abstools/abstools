package ABS.Meta;
// abslang.abs:1052:0: 
public interface ProductLine_i extends abs.backend.java.lib.types.ABSInterface {
    // abslang.abs:1053:4: 
    public  abs.backend.java.lib.runtime.ABSFut<ABS.Meta.Product_i> async_getCurrentProduct();
    // abslang.abs:1053:4: 
    public  ABS.Meta.Product_i getCurrentProduct();
    // abslang.abs:1054:4: 
    public  abs.backend.java.lib.runtime.ABSFut<ABS.Meta.Product_i> async_getProduct(abs.backend.java.lib.types.ABSString s);
    // abslang.abs:1054:4: 
    public  ABS.Meta.Product_i getProduct(abs.backend.java.lib.types.ABSString s);
    // abslang.abs:1055:4: 
    public  abs.backend.java.lib.runtime.ABSFut<abs.backend.java.lib.types.ABSUnit> async_reconfigure(ABS.Meta.Product_i p);
    // abslang.abs:1055:4: 
    public  abs.backend.java.lib.types.ABSUnit reconfigure(ABS.Meta.Product_i p);
    // abslang.abs:1056:4: 
    public  abs.backend.java.lib.runtime.ABSFut<abs.backend.java.lib.types.ABSUnit> async_addProduct(ABS.Meta.Product_i p);
    // abslang.abs:1056:4: 
    public  abs.backend.java.lib.types.ABSUnit addProduct(ABS.Meta.Product_i p);
    // abslang.abs:1057:4: 
    public  abs.backend.java.lib.runtime.ABSFut<abs.backend.java.lib.types.ABSUnit> async_removeProduct(ABS.Meta.Product_i p);
    // abslang.abs:1057:4: 
    public  abs.backend.java.lib.types.ABSUnit removeProduct(ABS.Meta.Product_i p);
    // abslang.abs:1058:4: 
    public  abs.backend.java.lib.runtime.ABSFut<abs.backend.java.lib.types.ABSUnit> async_addReconfiguration(ABS.Meta.Reconfiguration_i r);
    // abslang.abs:1058:4: 
    public  abs.backend.java.lib.types.ABSUnit addReconfiguration(ABS.Meta.Reconfiguration_i r);
    // abslang.abs:1059:4: 
    public  abs.backend.java.lib.runtime.ABSFut<abs.backend.java.lib.types.ABSUnit> async_removeReconfiguration(ABS.Meta.Reconfiguration_i r);
    // abslang.abs:1059:4: 
    public  abs.backend.java.lib.types.ABSUnit removeReconfiguration(ABS.Meta.Reconfiguration_i r);
}
