package ABS.StdLib;
// abslang.abs:0:0: 
public final class durationValue_f implements abs.backend.java.lib.types.ABSFunction {
    private durationValue_f() { }
    public static abs.backend.java.lib.types.ABSRational apply(ABS.StdLib.Duration data) {
        return (new abs.backend.java.lib.expr.Case() {
            public abs.backend.java.lib.types.ABSRational of(final ABS.StdLib.Duration data, final ABS.StdLib.Duration __ABS_value) {
                final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternConstructor(ABS.StdLib.Duration_Duration.class,new abs.backend.java.lib.expr.PatternVariable("res")).match(__ABS_value);
                if (__ABS_binding0 != null) return new Object() {
                    public abs.backend.java.lib.types.ABSRational execute(final abs.backend.java.lib.types.ABSRational res) { return res; }
                }.execute((abs.backend.java.lib.types.ABSRational) __ABS_binding0.getBinding(0));
                throw new abs.backend.java.lib.expr.UnmatchedCaseException("abslang.abs:0:0:  value " + __ABS_value + " did not match any pattern.");
            }
        }.of(data, data));
    }
}
