package ABS.StdLib;
// abslang.abs:77:0: 
public final class min_f implements abs.backend.java.lib.types.ABSFunction {
    private min_f() { }
    public static <A extends abs.backend.java.lib.types.ABSValue>A apply(A a, A b) {
        return (abs.backend.java.lib.expr.BinOp.lt(a,b).toBoolean() ? a : b);
    }
}
