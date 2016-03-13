package ABS.DC;
// abslang.abs:670:0: 
public interface CloudProvider_i extends abs.backend.java.lib.types.ABSInterface {
    // abslang.abs:674:4: 
    public  abs.backend.java.lib.runtime.ABSFut<ABS.DC.DeploymentComponent_i> async_prelaunchInstance(ABS.StdLib.Map<ABS.DC.Resourcetype,abs.backend.java.lib.types.ABSRational> d);
    // abslang.abs:674:4: 
    public  ABS.DC.DeploymentComponent_i prelaunchInstance(ABS.StdLib.Map<ABS.DC.Resourcetype,abs.backend.java.lib.types.ABSRational> d);
    // abslang.abs:675:4: 
    public  abs.backend.java.lib.runtime.ABSFut<ABS.DC.DeploymentComponent_i> async_launchInstance(ABS.StdLib.Map<ABS.DC.Resourcetype,abs.backend.java.lib.types.ABSRational> description);
    // abslang.abs:675:4: 
    public  ABS.DC.DeploymentComponent_i launchInstance(ABS.StdLib.Map<ABS.DC.Resourcetype,abs.backend.java.lib.types.ABSRational> description);
    // abslang.abs:678:4: 
    public  abs.backend.java.lib.runtime.ABSFut<abs.backend.java.lib.types.ABSBool> async_acquireInstance(ABS.DC.DeploymentComponent_i instance);
    // abslang.abs:678:4: 
    public  abs.backend.java.lib.types.ABSBool acquireInstance(ABS.DC.DeploymentComponent_i instance);
    // abslang.abs:679:4: 
    public  abs.backend.java.lib.runtime.ABSFut<abs.backend.java.lib.types.ABSBool> async_releaseInstance(ABS.DC.DeploymentComponent_i instance);
    // abslang.abs:679:4: 
    public  abs.backend.java.lib.types.ABSBool releaseInstance(ABS.DC.DeploymentComponent_i instance);
    // abslang.abs:680:4: 
    public  abs.backend.java.lib.runtime.ABSFut<abs.backend.java.lib.types.ABSBool> async_killInstance(ABS.DC.DeploymentComponent_i instance);
    // abslang.abs:680:4: 
    public  abs.backend.java.lib.types.ABSBool killInstance(ABS.DC.DeploymentComponent_i instance);
    // abslang.abs:682:4: 
    public  abs.backend.java.lib.runtime.ABSFut<abs.backend.java.lib.types.ABSRational> async_getAccumulatedCost();
    // abslang.abs:682:4: 
    public  abs.backend.java.lib.types.ABSRational getAccumulatedCost();
    // abslang.abs:683:4: 
    public  abs.backend.java.lib.runtime.ABSFut<abs.backend.java.lib.types.ABSUnit> async_shutdown();
    // abslang.abs:683:4: 
    public  abs.backend.java.lib.types.ABSUnit shutdown();
}
