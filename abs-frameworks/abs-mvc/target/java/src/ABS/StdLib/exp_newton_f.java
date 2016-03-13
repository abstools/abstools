package ABS.StdLib;
// abslang.abs:101:0: 
public final class exp_newton_f implements abs.backend.java.lib.types.ABSFunction {
    private exp_newton_f() { }
    public static abs.backend.java.lib.types.ABSRational apply(abs.backend.java.lib.types.ABSRational x, abs.backend.java.lib.types.ABSRational epsilon) {
        return (abs.backend.java.lib.expr.BinOp.lt(x,abs.backend.java.lib.types.ABSInteger.fromString("0")).toBoolean() ? abs.backend.java.lib.types.ABSInteger.fromString("1").divide(ABS.StdLib.exp_newton_helper_f.apply(abs.backend.java.lib.types.ABSInteger.fromString("1").subtract(x).add(x.multiply(x).divide(abs.backend.java.lib.types.ABSInteger.fromString("2"))), x.negate(), abs.backend.java.lib.types.ABSInteger.fromString("3"), x.multiply(x), abs.backend.java.lib.types.ABSInteger.fromString("2"), epsilon)) : ABS.StdLib.exp_newton_helper_f.apply(abs.backend.java.lib.types.ABSInteger.fromString("1").add(x).add(x.multiply(x).divide(abs.backend.java.lib.types.ABSInteger.fromString("2"))), x, abs.backend.java.lib.types.ABSInteger.fromString("3"), x.multiply(x), abs.backend.java.lib.types.ABSInteger.fromString("2"), epsilon));
    }
}
