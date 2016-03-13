package ABS.StdLib;
// abslang.abs:257:0: 
public final class next_f implements abs.backend.java.lib.types.ABSFunction {
    private next_f() { }
    public static <A extends abs.backend.java.lib.types.ABSValue>ABS.StdLib.Pair<ABS.StdLib.Set<A>,A> apply(ABS.StdLib.Set<A> s) {
        return (new abs.backend.java.lib.expr.Case() {
            public ABS.StdLib.Pair<ABS.StdLib.Set<A>,A> of(final ABS.StdLib.Set<A> s, final ABS.StdLib.Set<A> __ABS_value) {
                final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternConstructor(ABS.StdLib.Set_Insert.class,new abs.backend.java.lib.expr.PatternVariable("e"),new abs.backend.java.lib.expr.PatternVariable("set2")).match(__ABS_value);
                if (__ABS_binding0 != null) return new Object() {
                    public ABS.StdLib.Pair<ABS.StdLib.Set<A>,A> execute(final A e, final ABS.StdLib.Set<A> set2) { return new ABS.StdLib.Pair_Pair(set2, e); }
                }.execute((A) __ABS_binding0.getBinding(0),(ABS.StdLib.Set<A>) __ABS_binding0.getBinding(1));
                throw new abs.backend.java.lib.expr.UnmatchedCaseException("abslang.abs:258:3:  value " + __ABS_value + " did not match any pattern.");
            }
        }.of(s, s));
    }
}
