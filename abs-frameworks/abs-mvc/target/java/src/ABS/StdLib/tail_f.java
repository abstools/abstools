package ABS.StdLib;
// abslang.abs:0:0: 
public final class tail_f implements abs.backend.java.lib.types.ABSFunction {
    private tail_f() { }
    public static <A extends abs.backend.java.lib.types.ABSValue>ABS.StdLib.List<A> apply(ABS.StdLib.List<A> data) {
        return (new abs.backend.java.lib.expr.Case() {
            public ABS.StdLib.List<A> of(final ABS.StdLib.List<A> data, final ABS.StdLib.List<A> __ABS_value) {
                final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternConstructor(ABS.StdLib.List_Cons.class,new abs.backend.java.lib.expr.AnyPattern(),new abs.backend.java.lib.expr.PatternVariable("res")).match(__ABS_value);
                if (__ABS_binding0 != null) return new Object() {
                    public ABS.StdLib.List<A> execute(final ABS.StdLib.List<A> res) { return res; }
                }.execute((ABS.StdLib.List<A>) __ABS_binding0.getBinding(0));
                throw new abs.backend.java.lib.expr.UnmatchedCaseException("abslang.abs:0:0:  value " + __ABS_value + " did not match any pattern.");
            }
        }.of(data, data));
    }
}
