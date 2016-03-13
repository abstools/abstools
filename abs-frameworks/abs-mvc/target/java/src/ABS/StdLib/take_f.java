package ABS.StdLib;
// abslang.abs:248:0: 
public final class take_f implements abs.backend.java.lib.types.ABSFunction {
    private take_f() { }
    public static <A extends abs.backend.java.lib.types.ABSValue>A apply(ABS.StdLib.Set<A> ss) {
        return (new abs.backend.java.lib.expr.Case() {
            public A of(final ABS.StdLib.Set<A> ss, final ABS.StdLib.Set<A> __ABS_value) {
                final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternConstructor(ABS.StdLib.Set_Insert.class,new abs.backend.java.lib.expr.PatternVariable("e"),new abs.backend.java.lib.expr.AnyPattern()).match(__ABS_value);
                if (__ABS_binding0 != null) return new Object() {
                    public A execute(final A e) { return e; }
                }.execute((A) __ABS_binding0.getBinding(0));
                throw new abs.backend.java.lib.expr.UnmatchedCaseException("abslang.abs:249:2:  value " + __ABS_value + " did not match any pattern.");
            }
        }.of(ss, ss));
    }
}
