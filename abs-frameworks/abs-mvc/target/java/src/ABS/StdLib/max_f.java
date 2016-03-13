package ABS.StdLib;
// abslang.abs:76:0: 
public final class max_f implements abs.backend.java.lib.types.ABSFunction {
    private max_f() { }
    public static <A extends abs.backend.java.lib.types.ABSValue>A apply(A a, A b) {
        return (abs.backend.java.lib.expr.BinOp.gt(a,b).toBoolean() ? a : b);
    }
}
