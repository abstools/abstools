package ABS.StdLib;
// abslang.abs:270:0: 
public final class length_f implements abs.backend.java.lib.types.ABSFunction {
    private length_f() { }
    public static <A extends abs.backend.java.lib.types.ABSValue>abs.backend.java.lib.types.ABSInteger apply(ABS.StdLib.List<A> list) {
        return (new abs.backend.java.lib.expr.Case() {
            public abs.backend.java.lib.types.ABSInteger of(final ABS.StdLib.List<A> list, final ABS.StdLib.List<A> __ABS_value) {
                final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternConstructor(ABS.StdLib.List_Nil.class).match(__ABS_value);
                if (__ABS_binding0 != null) return new Object() {
                    public abs.backend.java.lib.types.ABSInteger execute() { return abs.backend.java.lib.types.ABSInteger.fromString("0"); }
                }.execute();
                final abs.backend.java.lib.expr.PatternBinding __ABS_binding1 = new abs.backend.java.lib.expr.PatternConstructor(ABS.StdLib.List_Cons.class,new abs.backend.java.lib.expr.PatternVariable("p"),new abs.backend.java.lib.expr.PatternVariable("l")).match(__ABS_value);
                if (__ABS_binding1 != null) return new Object() {
                    public abs.backend.java.lib.types.ABSInteger execute(final A p, final ABS.StdLib.List<A> l) { return abs.backend.java.lib.types.ABSInteger.fromString("1").add(ABS.StdLib.length_f.apply(l)); }
                }.execute((A) __ABS_binding1.getBinding(0),(ABS.StdLib.List<A>) __ABS_binding1.getBinding(1));
                throw new abs.backend.java.lib.expr.UnmatchedCaseException("abslang.abs:271:3:  value " + __ABS_value + " did not match any pattern.");
            }
        }.of(list, list));
    }
}
