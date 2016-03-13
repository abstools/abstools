package ABS.DC;
// abslang.abs:638:0: 
public interface DeploymentComponent_i extends abs.backend.java.lib.types.ABSInterface {
    // abslang.abs:641:4: 
    public  abs.backend.java.lib.runtime.ABSFut<abs.backend.java.lib.types.ABSRational> async_load(ABS.DC.Resourcetype rtype, abs.backend.java.lib.types.ABSInteger periods);
    // abslang.abs:641:4: 
    public  abs.backend.java.lib.types.ABSRational load(ABS.DC.Resourcetype rtype, abs.backend.java.lib.types.ABSInteger periods);
    // abslang.abs:643:4: 
    public  abs.backend.java.lib.runtime.ABSFut<ABS.DC.InfRat> async_total(ABS.DC.Resourcetype rtype);
    // abslang.abs:643:4: 
    public  ABS.DC.InfRat total(ABS.DC.Resourcetype rtype);
    // abslang.abs:646:4: 
    public  abs.backend.java.lib.runtime.ABSFut<abs.backend.java.lib.types.ABSUnit> async_transfer(ABS.DC.DeploymentComponent_i target, abs.backend.java.lib.types.ABSRational amount, ABS.DC.Resourcetype rtype);
    // abslang.abs:646:4: 
    public  abs.backend.java.lib.types.ABSUnit transfer(ABS.DC.DeploymentComponent_i target, abs.backend.java.lib.types.ABSRational amount, ABS.DC.Resourcetype rtype);
    // abslang.abs:649:4: 
    public  abs.backend.java.lib.runtime.ABSFut<abs.backend.java.lib.types.ABSUnit> async_decrementResources(abs.backend.java.lib.types.ABSRational amount, ABS.DC.Resourcetype rtype);
    // abslang.abs:649:4: 
    public  abs.backend.java.lib.types.ABSUnit decrementResources(abs.backend.java.lib.types.ABSRational amount, ABS.DC.Resourcetype rtype);
    // abslang.abs:652:4: 
    public  abs.backend.java.lib.runtime.ABSFut<abs.backend.java.lib.types.ABSUnit> async_incrementResources(abs.backend.java.lib.types.ABSRational amount, ABS.DC.Resourcetype rtype);
    // abslang.abs:652:4: 
    public  abs.backend.java.lib.types.ABSUnit incrementResources(abs.backend.java.lib.types.ABSRational amount, ABS.DC.Resourcetype rtype);
    // abslang.abs:654:4: 
    public  abs.backend.java.lib.runtime.ABSFut<abs.backend.java.lib.types.ABSUnit> async_setProvider(ABS.DC.CloudProvider_i provider);
    // abslang.abs:654:4: 
    public  abs.backend.java.lib.types.ABSUnit setProvider(ABS.DC.CloudProvider_i provider);
    // abslang.abs:655:4: 
    public  abs.backend.java.lib.runtime.ABSFut<ABS.DC.CloudProvider_i> async_getProvider();
    // abslang.abs:655:4: 
    public  ABS.DC.CloudProvider_i getProvider();
    // abslang.abs:656:4: 
    public  abs.backend.java.lib.runtime.ABSFut<ABS.StdLib.Time> async_getCreationTime();
    // abslang.abs:656:4: 
    public  ABS.StdLib.Time getCreationTime();
    // abslang.abs:657:4: 
    public  abs.backend.java.lib.runtime.ABSFut<abs.backend.java.lib.types.ABSRational> async_getStartupDuration();
    // abslang.abs:657:4: 
    public  abs.backend.java.lib.types.ABSRational getStartupDuration();
    // abslang.abs:658:4: 
    public  abs.backend.java.lib.runtime.ABSFut<abs.backend.java.lib.types.ABSInteger> async_getPaymentInterval();
    // abslang.abs:658:4: 
    public  abs.backend.java.lib.types.ABSInteger getPaymentInterval();
    // abslang.abs:659:4: 
    public  abs.backend.java.lib.runtime.ABSFut<abs.backend.java.lib.types.ABSRational> async_getCostPerInterval();
    // abslang.abs:659:4: 
    public  abs.backend.java.lib.types.ABSRational getCostPerInterval();
    // abslang.abs:662:4: 
    public  abs.backend.java.lib.runtime.ABSFut<abs.backend.java.lib.types.ABSBool> async_acquire();
    // abslang.abs:662:4: 
    public  abs.backend.java.lib.types.ABSBool acquire();
    // abslang.abs:663:4: 
    public  abs.backend.java.lib.runtime.ABSFut<abs.backend.java.lib.types.ABSBool> async_release();
    // abslang.abs:663:4: 
    public  abs.backend.java.lib.types.ABSBool release();
    // abslang.abs:667:4: 
    public  abs.backend.java.lib.runtime.ABSFut<abs.backend.java.lib.types.ABSBool> async_matchesDescription(ABS.StdLib.Map<ABS.DC.Resourcetype,abs.backend.java.lib.types.ABSRational> description);
    // abslang.abs:667:4: 
    public  abs.backend.java.lib.types.ABSBool matchesDescription(ABS.StdLib.Map<ABS.DC.Resourcetype,abs.backend.java.lib.types.ABSRational> description);
}
