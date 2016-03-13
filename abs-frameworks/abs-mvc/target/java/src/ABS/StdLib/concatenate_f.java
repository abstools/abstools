package ABS.StdLib;
// abslang.abs:304:0: 
public final class concatenate_f implements abs.backend.java.lib.types.ABSFunction {
    private concatenate_f() { }
    public static <A extends abs.backend.java.lib.types.ABSValue>ABS.StdLib.List<A> apply(ABS.StdLib.List<A> list1, ABS.StdLib.List<A> list2) {
        return (new abs.backend.java.lib.expr.Case() {
            public ABS.StdLib.List<A> of(final ABS.StdLib.List<A> list1, final ABS.StdLib.List<A> list2, final ABS.StdLib.List<A> __ABS_value) {
                final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternConstructor(ABS.StdLib.List_Nil.class).match(__ABS_value);
                if (__ABS_binding0 != null) return new Object() {
                    public ABS.StdLib.List<A> execute() { return list2; }
                }.execute();
                final abs.backend.java.lib.expr.PatternBinding __ABS_binding1 = new abs.backend.java.lib.expr.PatternConstructor(ABS.StdLib.List_Cons.class,new abs.backend.java.lib.expr.PatternVariable("head"),new abs.backend.java.lib.expr.PatternVariable("tail")).match(__ABS_value);
                if (__ABS_binding1 != null) return new Object() {
                    public ABS.StdLib.List<A> execute(final A head, final ABS.StdLib.List<A> tail) { return new ABS.StdLib.List_Cons(head, ABS.StdLib.concatenate_f.apply(tail, list2)); }
                }.execute((A) __ABS_binding1.getBinding(0),(ABS.StdLib.List<A>) __ABS_binding1.getBinding(1));
                throw new abs.backend.java.lib.expr.UnmatchedCaseException("abslang.abs:305:2:  value " + __ABS_value + " did not match any pattern.");
            }
        }.of(list1, list2, list1));
    }
}
