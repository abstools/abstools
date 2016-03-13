package ABS.StdLib;
// abslang.abs:410:0: 
public final class put_f implements abs.backend.java.lib.types.ABSFunction {
    private put_f() { }
    public static <A extends abs.backend.java.lib.types.ABSValue,B extends abs.backend.java.lib.types.ABSValue>ABS.StdLib.Map<A,B> apply(ABS.StdLib.Map<A,B> ms, A k, B v) {
        return (new abs.backend.java.lib.expr.Case() {
            public ABS.StdLib.Map<A,B> of(final ABS.StdLib.Map<A,B> ms, final B v, final A k, final ABS.StdLib.Map<A,B> __ABS_value) {
                final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternConstructor(ABS.StdLib.Map_EmptyMap.class).match(__ABS_value);
                if (__ABS_binding0 != null) return new Object() {
                    public ABS.StdLib.Map<A,B> execute() { return new ABS.StdLib.Map_InsertAssoc(new ABS.StdLib.Pair_Pair(k, v), new ABS.StdLib.Map_EmptyMap()); }
                }.execute();
                final abs.backend.java.lib.expr.PatternBinding __ABS_binding1 = new abs.backend.java.lib.expr.PatternConstructor(ABS.StdLib.Map_InsertAssoc.class,new abs.backend.java.lib.expr.PatternConstructor(ABS.StdLib.Pair_Pair.class,new abs.backend.java.lib.expr.PatternValue(k),new abs.backend.java.lib.expr.AnyPattern()),new abs.backend.java.lib.expr.PatternVariable("ts")).match(__ABS_value);
                if (__ABS_binding1 != null) return new Object() {
                    public ABS.StdLib.Map<A,B> execute(final ABS.StdLib.Map<A,B> ts) { return new ABS.StdLib.Map_InsertAssoc(new ABS.StdLib.Pair_Pair(k, v), ts); }
                }.execute((ABS.StdLib.Map<A,B>) __ABS_binding1.getBinding(0));
                final abs.backend.java.lib.expr.PatternBinding __ABS_binding2 = new abs.backend.java.lib.expr.PatternConstructor(ABS.StdLib.Map_InsertAssoc.class,new abs.backend.java.lib.expr.PatternVariable("p"),new abs.backend.java.lib.expr.PatternVariable("ts")).match(__ABS_value);
                if (__ABS_binding2 != null) return new Object() {
                    public ABS.StdLib.Map<A,B> execute(final ABS.StdLib.Pair<A,B> p, final ABS.StdLib.Map<A,B> ts) { return new ABS.StdLib.Map_InsertAssoc(p, ABS.StdLib.put_f.apply(ts, k, v)); }
                }.execute((ABS.StdLib.Pair<A,B>) __ABS_binding2.getBinding(0),(ABS.StdLib.Map<A,B>) __ABS_binding2.getBinding(1));
                throw new abs.backend.java.lib.expr.UnmatchedCaseException("abslang.abs:411:2:  value " + __ABS_value + " did not match any pattern.");
            }
        }.of(ms, v, k, ms));
    }
}
