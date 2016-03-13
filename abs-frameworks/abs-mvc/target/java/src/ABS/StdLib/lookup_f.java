package ABS.StdLib;
// abslang.abs:370:0: 
public final class lookup_f implements abs.backend.java.lib.types.ABSFunction {
    private lookup_f() { }
    public static <A extends abs.backend.java.lib.types.ABSValue,B extends abs.backend.java.lib.types.ABSValue>ABS.StdLib.Maybe<B> apply(ABS.StdLib.Map<A,B> ms, A k) {
        return (new abs.backend.java.lib.expr.Case() {
            public ABS.StdLib.Maybe<B> of(final ABS.StdLib.Map<A,B> ms, final A k, final ABS.StdLib.Map<A,B> __ABS_value) {
                final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternConstructor(ABS.StdLib.Map_InsertAssoc.class,new abs.backend.java.lib.expr.PatternConstructor(ABS.StdLib.Pair_Pair.class,new abs.backend.java.lib.expr.PatternValue(k),new abs.backend.java.lib.expr.PatternVariable("y")),new abs.backend.java.lib.expr.AnyPattern()).match(__ABS_value);
                if (__ABS_binding0 != null) return new Object() {
                    public ABS.StdLib.Maybe<B> execute(final B y) { return new ABS.StdLib.Maybe_Just(y); }
                }.execute((B) __ABS_binding0.getBinding(0));
                final abs.backend.java.lib.expr.PatternBinding __ABS_binding1 = new abs.backend.java.lib.expr.PatternConstructor(ABS.StdLib.Map_InsertAssoc.class,new abs.backend.java.lib.expr.AnyPattern(),new abs.backend.java.lib.expr.PatternVariable("tm")).match(__ABS_value);
                if (__ABS_binding1 != null) return new Object() {
                    public ABS.StdLib.Maybe<B> execute(final ABS.StdLib.Map<A,B> tm) { return ABS.StdLib.lookup_f.apply(tm, k); }
                }.execute((ABS.StdLib.Map<A,B>) __ABS_binding1.getBinding(0));
                final abs.backend.java.lib.expr.PatternBinding __ABS_binding2 = new abs.backend.java.lib.expr.PatternConstructor(ABS.StdLib.Map_EmptyMap.class).match(__ABS_value);
                if (__ABS_binding2 != null) return new Object() {
                    public ABS.StdLib.Maybe<B> execute() { return new ABS.StdLib.Maybe_Nothing(); }
                }.execute();
                throw new abs.backend.java.lib.expr.UnmatchedCaseException("abslang.abs:371:2:  value " + __ABS_value + " did not match any pattern.");
            }
        }.of(ms, k, ms));
    }
}
