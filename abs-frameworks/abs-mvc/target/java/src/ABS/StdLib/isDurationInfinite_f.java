package ABS.StdLib;
// abslang.abs:474:0: 
public final class isDurationInfinite_f implements abs.backend.java.lib.types.ABSFunction {
    private isDurationInfinite_f() { }
    public static abs.backend.java.lib.types.ABSBool apply(ABS.StdLib.Duration d) {
        return (new abs.backend.java.lib.expr.Case() {
            public abs.backend.java.lib.types.ABSBool of(final ABS.StdLib.Duration d, final ABS.StdLib.Duration __ABS_value) {
                final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternConstructor(ABS.StdLib.Duration_Duration.class,new abs.backend.java.lib.expr.AnyPattern()).match(__ABS_value);
                if (__ABS_binding0 != null) return new Object() {
                    public abs.backend.java.lib.types.ABSBool execute() { return abs.backend.java.lib.types.ABSBool.FALSE; }
                }.execute();
                final abs.backend.java.lib.expr.PatternBinding __ABS_binding1 = new abs.backend.java.lib.expr.PatternConstructor(ABS.StdLib.Duration_InfDuration.class).match(__ABS_value);
                if (__ABS_binding1 != null) return new Object() {
                    public abs.backend.java.lib.types.ABSBool execute() { return abs.backend.java.lib.types.ABSBool.TRUE; }
                }.execute();
                throw new abs.backend.java.lib.expr.UnmatchedCaseException("abslang.abs:475:2:  value " + __ABS_value + " did not match any pattern.");
            }
        }.of(d, d));
    }
}
