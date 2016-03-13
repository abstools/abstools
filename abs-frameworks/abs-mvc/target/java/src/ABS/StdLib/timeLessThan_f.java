package ABS.StdLib;
// abslang.abs:470:0: 
public final class timeLessThan_f implements abs.backend.java.lib.types.ABSFunction {
    private timeLessThan_f() { }
    public static abs.backend.java.lib.types.ABSBool apply(ABS.StdLib.Time t1, ABS.StdLib.Time t2) {
        return (abs.backend.java.lib.expr.BinOp.lt(ABS.StdLib.timeValue_f.apply(t1),ABS.StdLib.timeValue_f.apply(t2)));
    }
}
