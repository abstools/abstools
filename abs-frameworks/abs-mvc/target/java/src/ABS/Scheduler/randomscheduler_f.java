package ABS.Scheduler;
// abslang.abs:595:0: 
public final class randomscheduler_f implements abs.backend.java.lib.types.ABSFunction {
    private randomscheduler_f() { }
    public static abs.backend.java.lib.types.ABSProcess apply(ABS.StdLib.List<abs.backend.java.lib.types.ABSProcess> queue) {
        return (ABS.StdLib.nth_f.apply(queue, abs.backend.java.lib.runtime.ABSBuiltInFunctions.random(ABS.StdLib.length_f.apply(queue))));
    }
}
