package ABS.StdLib;
// abslang.abs:167:0: 
public final class size_f implements abs.backend.java.lib.types.ABSFunction {
    private size_f() { }
    public static <A extends abs.backend.java.lib.types.ABSValue>abs.backend.java.lib.types.ABSInteger apply(ABS.StdLib.Set<A> xs) {
        return (new abs.backend.java.lib.expr.Case() {
            public abs.backend.java.lib.types.ABSInteger of(final ABS.StdLib.Set<A> xs, final ABS.StdLib.Set<A> __ABS_value) {
                final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternConstructor(ABS.StdLib.Set_EmptySet.class).match(__ABS_value);
                if (__ABS_binding0 != null) return new Object() {
                    public abs.backend.java.lib.types.ABSInteger execute() { return abs.backend.java.lib.types.ABSInteger.fromString("0"); }
                }.execute();
                final abs.backend.java.lib.expr.PatternBinding __ABS_binding1 = new abs.backend.java.lib.expr.PatternConstructor(ABS.StdLib.Set_Insert.class,new abs.backend.java.lib.expr.PatternVariable("s"),new abs.backend.java.lib.expr.PatternVariable("ss")).match(__ABS_value);
                if (__ABS_binding1 != null) return new Object() {
                    public abs.backend.java.lib.types.ABSInteger execute(final A s, final ABS.StdLib.Set<A> ss) { return abs.backend.java.lib.types.ABSInteger.fromString("1").add(ABS.StdLib.size_f.apply(ss)); }
                }.execute((A) __ABS_binding1.getBinding(0),(ABS.StdLib.Set<A>) __ABS_binding1.getBinding(1));
                throw new abs.backend.java.lib.expr.UnmatchedCaseException("abslang.abs:168:3:  value " + __ABS_value + " did not match any pattern.");
            }
        }.of(xs, xs));
    }
}
