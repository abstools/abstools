package ABS.StdLib;
// abslang.abs:0:0: 
public final class fstT_f implements abs.backend.java.lib.types.ABSFunction {
    private fstT_f() { }
    public static <A extends abs.backend.java.lib.types.ABSValue,B extends abs.backend.java.lib.types.ABSValue,C extends abs.backend.java.lib.types.ABSValue>A apply(ABS.StdLib.Triple<A,B,C> data) {
        return (new abs.backend.java.lib.expr.Case() {
            public A of(final ABS.StdLib.Triple<A,B,C> data, final ABS.StdLib.Triple<A,B,C> __ABS_value) {
                final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternConstructor(ABS.StdLib.Triple_Triple.class,new abs.backend.java.lib.expr.PatternVariable("res"),new abs.backend.java.lib.expr.AnyPattern(),new abs.backend.java.lib.expr.AnyPattern()).match(__ABS_value);
                if (__ABS_binding0 != null) return new Object() {
                    public A execute(final A res) { return res; }
                }.execute((A) __ABS_binding0.getBinding(0));
                throw new abs.backend.java.lib.expr.UnmatchedCaseException("abslang.abs:0:0:  value " + __ABS_value + " did not match any pattern.");
            }
        }.of(data, data));
    }
}
