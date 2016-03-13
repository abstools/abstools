package ABS.StdLib;
// abslang.abs:480:0: 
public final class subtractDuration_f implements abs.backend.java.lib.types.ABSFunction {
    private subtractDuration_f() { }
    public static ABS.StdLib.Time apply(ABS.StdLib.Time t, ABS.StdLib.Duration d) {
        return (new ABS.StdLib.Time_Time(ABS.StdLib.timeValue_f.apply(t).subtract(ABS.StdLib.durationValue_f.apply(d))));
    }
}
