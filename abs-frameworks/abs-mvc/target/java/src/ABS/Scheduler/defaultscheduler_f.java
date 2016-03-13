package ABS.Scheduler;
// abslang.abs:593:0: 
public final class defaultscheduler_f implements abs.backend.java.lib.types.ABSFunction {
    private defaultscheduler_f() { }
    public static abs.backend.java.lib.types.ABSProcess apply(ABS.StdLib.List<abs.backend.java.lib.types.ABSProcess> queue) {
        return (ABS.StdLib.head_f.apply(queue));
    }
}
