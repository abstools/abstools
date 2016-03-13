package ABS.StdLib;
// abslang.abs:89:0: 
public final class sqrt_newton_f implements abs.backend.java.lib.types.ABSFunction {
    private sqrt_newton_f() { }
    public static abs.backend.java.lib.types.ABSRational apply(abs.backend.java.lib.types.ABSRational x, abs.backend.java.lib.types.ABSRational estimate, abs.backend.java.lib.types.ABSRational epsilon) {
        return (new abs.backend.java.lib.expr.Let() { public abs.backend.java.lib.types.ABSRational in(final abs.backend.java.lib.types.ABSRational epsilon, final abs.backend.java.lib.types.ABSRational estimate, final abs.backend.java.lib.types.ABSRational x, final abs.backend.java.lib.types.ABSRational next) { return abs.backend.java.lib.expr.BinOp.lt(ABS.StdLib.abs___f.apply(estimate.subtract(next)),epsilon).toBoolean() ? estimate : ABS.StdLib.sqrt_newton_f.apply(x, next, epsilon); }}.in(epsilon, estimate, x, estimate.add(x.divide(estimate)).divide(abs.backend.java.lib.types.ABSInteger.fromString("2"))));
    }
}
