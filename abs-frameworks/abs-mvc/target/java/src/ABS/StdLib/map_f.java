package ABS.StdLib;
// abslang.abs:337:0: 
public final class map_f implements abs.backend.java.lib.types.ABSFunction {
    private map_f() { }
    public static <A extends abs.backend.java.lib.types.ABSValue,B extends abs.backend.java.lib.types.ABSValue>ABS.StdLib.Map<A,B> apply(ABS.StdLib.List<ABS.StdLib.Pair<A,B>> l) {
        return (new abs.backend.java.lib.expr.Case() {
            public ABS.StdLib.Map<A,B> of(final ABS.StdLib.List<ABS.StdLib.Pair<A,B>> l, final ABS.StdLib.List<ABS.StdLib.Pair<A,B>> __ABS_value) {
                final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternConstructor(ABS.StdLib.List_Nil.class).match(__ABS_value);
                if (__ABS_binding0 != null) return new Object() {
                    public ABS.StdLib.Map<A,B> execute() { return new ABS.StdLib.Map_EmptyMap(); }
                }.execute();
                final abs.backend.java.lib.expr.PatternBinding __ABS_binding1 = new abs.backend.java.lib.expr.PatternConstructor(ABS.StdLib.List_Cons.class,new abs.backend.java.lib.expr.PatternVariable("hd"),new abs.backend.java.lib.expr.PatternVariable("tl")).match(__ABS_value);
                if (__ABS_binding1 != null) return new Object() {
                    public ABS.StdLib.Map<A,B> execute(final ABS.StdLib.Pair<A,B> hd, final ABS.StdLib.List<ABS.StdLib.Pair<A,B>> tl) { return new ABS.StdLib.Map_InsertAssoc(hd, ABS.StdLib.map_f.apply(tl)); }
                }.execute((ABS.StdLib.Pair<A,B>) __ABS_binding1.getBinding(0),(ABS.StdLib.List<ABS.StdLib.Pair<A,B>>) __ABS_binding1.getBinding(1));
                throw new abs.backend.java.lib.expr.UnmatchedCaseException("abslang.abs:338:2:  value " + __ABS_value + " did not match any pattern.");
            }
        }.of(l, l));
    }
}
