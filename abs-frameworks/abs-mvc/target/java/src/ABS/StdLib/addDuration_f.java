package ABS.StdLib;
// abslang.abs:477:0: 
public final class addDuration_f implements abs.backend.java.lib.types.ABSFunction {
    private addDuration_f() { }
    public static ABS.StdLib.Time apply(ABS.StdLib.Time t, ABS.StdLib.Duration d) {
        return (new ABS.StdLib.Time_Time(ABS.StdLib.timeValue_f.apply(t).add(ABS.StdLib.durationValue_f.apply(d))));
    }
}
