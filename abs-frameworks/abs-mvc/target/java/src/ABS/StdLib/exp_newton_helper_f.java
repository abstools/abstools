package ABS.StdLib;
// abslang.abs:95:0: 
public final class exp_newton_helper_f implements abs.backend.java.lib.types.ABSFunction {
    private exp_newton_helper_f() { }
    public static abs.backend.java.lib.types.ABSRational apply(abs.backend.java.lib.types.ABSRational acc, abs.backend.java.lib.types.ABSRational x, abs.backend.java.lib.types.ABSInteger next_round, abs.backend.java.lib.types.ABSRational numerator, abs.backend.java.lib.types.ABSInteger denominator, abs.backend.java.lib.types.ABSRational epsilon) {
        return (new abs.backend.java.lib.expr.Let() { public abs.backend.java.lib.types.ABSRational in(final abs.backend.java.lib.types.ABSRational epsilon, final abs.backend.java.lib.types.ABSRational acc, final abs.backend.java.lib.types.ABSInteger next_round, final abs.backend.java.lib.types.ABSRational x, final abs.backend.java.lib.types.ABSRational numerator, final abs.backend.java.lib.types.ABSInteger denominator, final abs.backend.java.lib.types.ABSRational next) { return abs.backend.java.lib.expr.BinOp.lt(ABS.StdLib.abs___f.apply(next),epsilon).toBoolean() ? acc.add(next) : ABS.StdLib.exp_newton_helper_f.apply(acc.add(next), x, next_round.add(abs.backend.java.lib.types.ABSInteger.fromString("1")), numerator.multiply(x), denominator.multiply(next_round), epsilon); }}.in(epsilon, acc, next_round, x, numerator, denominator, numerator.multiply(x).divide(denominator.multiply(next_round))));
    }
}
