package ABS.StdLib;
// abslang.abs:81:0: 
public final class pow_f implements abs.backend.java.lib.types.ABSFunction {
    private pow_f() { }
    public static abs.backend.java.lib.types.ABSRational apply(abs.backend.java.lib.types.ABSRational b, abs.backend.java.lib.types.ABSInteger n) {
        return (abs.backend.java.lib.expr.BinOp.lt(n,abs.backend.java.lib.types.ABSInteger.fromString("0")).toBoolean() ? abs.backend.java.lib.types.ABSInteger.fromString("1").divide(ABS.StdLib.pow_f.apply(b, n.negate())) : new abs.backend.java.lib.expr.Case() {
            public abs.backend.java.lib.types.ABSRational of(final abs.backend.java.lib.types.ABSRational b, final abs.backend.java.lib.types.ABSInteger n, final abs.backend.java.lib.types.ABSInteger __ABS_value) {
                final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternValue(abs.backend.java.lib.types.ABSInteger.fromString("0")).match(__ABS_value);
                if (__ABS_binding0 != null) return new Object() {
                    public abs.backend.java.lib.types.ABSRational execute() { return abs.backend.java.lib.types.ABSInteger.fromString("1"); }
                }.execute();
                final abs.backend.java.lib.expr.PatternBinding __ABS_binding1 = new abs.backend.java.lib.expr.AnyPattern().match(__ABS_value);
                if (__ABS_binding1 != null) return new Object() {
                    public abs.backend.java.lib.types.ABSRational execute() { return b.multiply(ABS.StdLib.pow_f.apply(b, n.subtract(abs.backend.java.lib.types.ABSInteger.fromString("1")))); }
                }.execute();
                throw new abs.backend.java.lib.expr.UnmatchedCaseException("abslang.abs:84:9:  value " + __ABS_value + " did not match any pattern.");
            }
        }.of(b, n, n));
    }
}
