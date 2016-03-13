package ABS.StdLib;
// abslang.abs:483:0: 
public final class durationLessThan_f implements abs.backend.java.lib.types.ABSFunction {
    private durationLessThan_f() { }
    public static abs.backend.java.lib.types.ABSBool apply(ABS.StdLib.Duration d1, ABS.StdLib.Duration d2) {
        return (new abs.backend.java.lib.expr.Case() {
            public abs.backend.java.lib.types.ABSBool of(final ABS.StdLib.Duration d1, final ABS.StdLib.Duration d2, final ABS.StdLib.Duration __ABS_value) {
                final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternConstructor(ABS.StdLib.Duration_Duration.class,new abs.backend.java.lib.expr.PatternVariable("v1")).match(__ABS_value);
                if (__ABS_binding0 != null) return new Object() {
                    public abs.backend.java.lib.types.ABSBool execute(final abs.backend.java.lib.types.ABSRational v1) { return new abs.backend.java.lib.expr.Case() {
                        public abs.backend.java.lib.types.ABSBool of(final abs.backend.java.lib.types.ABSRational v1, final ABS.StdLib.Duration d2, final ABS.StdLib.Duration __ABS_value) {
                            final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternConstructor(ABS.StdLib.Duration_Duration.class,new abs.backend.java.lib.expr.PatternVariable("v2")).match(__ABS_value);
                            if (__ABS_binding0 != null) return new Object() {
                                public abs.backend.java.lib.types.ABSBool execute(final abs.backend.java.lib.types.ABSRational v2) { return abs.backend.java.lib.expr.BinOp.lt(v1,v2); }
                            }.execute((abs.backend.java.lib.types.ABSRational) __ABS_binding0.getBinding(0));
                            final abs.backend.java.lib.expr.PatternBinding __ABS_binding1 = new abs.backend.java.lib.expr.PatternConstructor(ABS.StdLib.Duration_InfDuration.class).match(__ABS_value);
                            if (__ABS_binding1 != null) return new Object() {
                                public abs.backend.java.lib.types.ABSBool execute() { return abs.backend.java.lib.types.ABSBool.FALSE; }
                            }.execute();
                            throw new abs.backend.java.lib.expr.UnmatchedCaseException("abslang.abs:485:22:  value " + __ABS_value + " did not match any pattern.");
                        }
                    }.of(v1, d2, d2); }
                }.execute((abs.backend.java.lib.types.ABSRational) __ABS_binding0.getBinding(0));
                final abs.backend.java.lib.expr.PatternBinding __ABS_binding1 = new abs.backend.java.lib.expr.PatternConstructor(ABS.StdLib.Duration_InfDuration.class).match(__ABS_value);
                if (__ABS_binding1 != null) return new Object() {
                    public abs.backend.java.lib.types.ABSBool execute() { return abs.backend.java.lib.types.ABSBool.FALSE; }
                }.execute();
                throw new abs.backend.java.lib.expr.UnmatchedCaseException("abslang.abs:484:2:  value " + __ABS_value + " did not match any pattern.");
            }
        }.of(d1, d2, d1));
    }
}
