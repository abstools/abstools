package ABS.StdLib;
// abslang.abs:500:0: 
public final class subtractFromDuration_f implements abs.backend.java.lib.types.ABSFunction {
    private subtractFromDuration_f() { }
    public static ABS.StdLib.Duration apply(ABS.StdLib.Duration d, abs.backend.java.lib.types.ABSRational v) {
        return (new abs.backend.java.lib.expr.Case() {
            public ABS.StdLib.Duration of(final ABS.StdLib.Duration d, final abs.backend.java.lib.types.ABSRational v, final ABS.StdLib.Duration __ABS_value) {
                final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternConstructor(ABS.StdLib.Duration_InfDuration.class).match(__ABS_value);
                if (__ABS_binding0 != null) return new Object() {
                    public ABS.StdLib.Duration execute() { return new ABS.StdLib.Duration_InfDuration(); }
                }.execute();
                final abs.backend.java.lib.expr.PatternBinding __ABS_binding1 = new abs.backend.java.lib.expr.PatternConstructor(ABS.StdLib.Duration_Duration.class,new abs.backend.java.lib.expr.PatternVariable("x")).match(__ABS_value);
                if (__ABS_binding1 != null) return new Object() {
                    public ABS.StdLib.Duration execute(final abs.backend.java.lib.types.ABSRational x) { return new ABS.StdLib.Duration_Duration(ABS.StdLib.max_f.apply(x.subtract(v), abs.backend.java.lib.types.ABSInteger.fromString("0"))); }
                }.execute((abs.backend.java.lib.types.ABSRational) __ABS_binding1.getBinding(0));
                throw new abs.backend.java.lib.expr.UnmatchedCaseException("abslang.abs:501:2:  value " + __ABS_value + " did not match any pattern.");
            }
        }.of(d, v, d));
    }
}
