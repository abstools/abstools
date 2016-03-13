package ABS.Meta;
// abslang.abs:1037:0: 
public interface ProcessScheduler_i extends abs.backend.java.lib.types.ABSInterface {
    // abslang.abs:1038:4: 
    public  abs.backend.java.lib.runtime.ABSFut<abs.backend.java.lib.types.ABSProcess> async_schedule(ABS.StdLib.List<abs.backend.java.lib.types.ABSProcess> queue);
    // abslang.abs:1038:4: 
    public  abs.backend.java.lib.types.ABSProcess schedule(ABS.StdLib.List<abs.backend.java.lib.types.ABSProcess> queue);
}
