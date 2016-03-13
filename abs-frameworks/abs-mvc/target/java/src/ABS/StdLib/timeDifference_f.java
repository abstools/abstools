package ABS.StdLib;
// abslang.abs:468:0: 
public final class timeDifference_f implements abs.backend.java.lib.types.ABSFunction {
    private timeDifference_f() { }
    public static abs.backend.java.lib.types.ABSRational apply(ABS.StdLib.Time t1, ABS.StdLib.Time t2) {
        return (ABS.StdLib.abs___f.apply(ABS.StdLib.timeValue_f.apply(t2).subtract(ABS.StdLib.timeValue_f.apply(t1))));
    }
}
