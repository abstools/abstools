package ABS.StdLib;
// abslang.abs:319:0: 
public final class reverse_f implements abs.backend.java.lib.types.ABSFunction {
    private reverse_f() { }
    public static <A extends abs.backend.java.lib.types.ABSValue>ABS.StdLib.List<A> apply(ABS.StdLib.List<A> list) {
        return (new abs.backend.java.lib.expr.Case() {
            public ABS.StdLib.List<A> of(final ABS.StdLib.List<A> list, final ABS.StdLib.List<A> __ABS_value) {
                final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternConstructor(ABS.StdLib.List_Cons.class,new abs.backend.java.lib.expr.PatternVariable("hd"),new abs.backend.java.lib.expr.PatternVariable("tl")).match(__ABS_value);
                if (__ABS_binding0 != null) return new Object() {
                    public ABS.StdLib.List<A> execute(final A hd, final ABS.StdLib.List<A> tl) { return ABS.StdLib.appendright_f.apply(ABS.StdLib.reverse_f.apply(tl), hd); }
                }.execute((A) __ABS_binding0.getBinding(0),(ABS.StdLib.List<A>) __ABS_binding0.getBinding(1));
                final abs.backend.java.lib.expr.PatternBinding __ABS_binding1 = new abs.backend.java.lib.expr.PatternConstructor(ABS.StdLib.List_Nil.class).match(__ABS_value);
                if (__ABS_binding1 != null) return new Object() {
                    public ABS.StdLib.List<A> execute() { return new ABS.StdLib.List_Nil(); }
                }.execute();
                throw new abs.backend.java.lib.expr.UnmatchedCaseException("abslang.abs:320:2:  value " + __ABS_value + " did not match any pattern.");
            }
        }.of(list, list));
    }
}
