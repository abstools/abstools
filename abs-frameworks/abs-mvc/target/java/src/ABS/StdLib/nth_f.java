package ABS.StdLib;
// abslang.abs:284:0: 
public final class nth_f implements abs.backend.java.lib.types.ABSFunction {
    private nth_f() { }
    public static <A extends abs.backend.java.lib.types.ABSValue>A apply(ABS.StdLib.List<A> list, abs.backend.java.lib.types.ABSInteger n) {
        return (new abs.backend.java.lib.expr.Case() {
            public A of(final ABS.StdLib.List<A> list, final abs.backend.java.lib.types.ABSInteger n, final abs.backend.java.lib.types.ABSInteger __ABS_value) {
                final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternValue(abs.backend.java.lib.types.ABSInteger.fromString("0")).match(__ABS_value);
                if (__ABS_binding0 != null) return new Object() {
                    public A execute() { return ABS.StdLib.head_f.apply(list); }
                }.execute();
                final abs.backend.java.lib.expr.PatternBinding __ABS_binding1 = new abs.backend.java.lib.expr.AnyPattern().match(__ABS_value);
                if (__ABS_binding1 != null) return new Object() {
                    public A execute() { return ABS.StdLib.nth_f.apply(ABS.StdLib.tail_f.apply(list), n.subtract(abs.backend.java.lib.types.ABSInteger.fromString("1"))); }
                }.execute();
                throw new abs.backend.java.lib.expr.UnmatchedCaseException("abslang.abs:285:2:  value " + __ABS_value + " did not match any pattern.");
            }
        }.of(list, n, n));
    }
}
