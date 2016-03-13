package ABS.StdLib;
// abslang.abs:464:0: 
public final class now_f implements abs.backend.java.lib.types.ABSFunction {
    private now_f() { }
    public static ABS.StdLib.Time apply() {
        return (new ABS.StdLib.Time_Time(abs.backend.java.lib.runtime.ABSBuiltInFunctions.currentms()));
    }
}
