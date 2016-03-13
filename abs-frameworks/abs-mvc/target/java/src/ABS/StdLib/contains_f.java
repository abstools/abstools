package ABS.StdLib;
// abslang.abs:152:0: 
public final class contains_f implements abs.backend.java.lib.types.ABSFunction {
    private contains_f() { }
    public static <A extends abs.backend.java.lib.types.ABSValue>abs.backend.java.lib.types.ABSBool apply(ABS.StdLib.Set<A> ss, A e) {
        return (new abs.backend.java.lib.expr.Case() {
            public abs.backend.java.lib.types.ABSBool of(final ABS.StdLib.Set<A> ss, final A e, final ABS.StdLib.Set<A> __ABS_value) {
                final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternConstructor(ABS.StdLib.Set_EmptySet.class).match(__ABS_value);
                if (__ABS_binding0 != null) return new Object() {
                    public abs.backend.java.lib.types.ABSBool execute() { return abs.backend.java.lib.types.ABSBool.FALSE; }
                }.execute();
                final abs.backend.java.lib.expr.PatternBinding __ABS_binding1 = new abs.backend.java.lib.expr.PatternConstructor(ABS.StdLib.Set_Insert.class,new abs.backend.java.lib.expr.PatternValue(e),new abs.backend.java.lib.expr.AnyPattern()).match(__ABS_value);
                if (__ABS_binding1 != null) return new Object() {
                    public abs.backend.java.lib.types.ABSBool execute() { return abs.backend.java.lib.types.ABSBool.TRUE; }
                }.execute();
                final abs.backend.java.lib.expr.PatternBinding __ABS_binding2 = new abs.backend.java.lib.expr.PatternConstructor(ABS.StdLib.Set_Insert.class,new abs.backend.java.lib.expr.PatternVariable("x"),new abs.backend.java.lib.expr.PatternVariable("xs")).match(__ABS_value);
                if (__ABS_binding2 != null) return new Object() {
                    public abs.backend.java.lib.types.ABSBool execute(final A x, final ABS.StdLib.Set<A> xs) { return abs.backend.java.lib.expr.BinOp.gt(x,e).toBoolean() ? abs.backend.java.lib.types.ABSBool.FALSE : ABS.StdLib.contains_f.apply(xs, e); }
                }.execute((A) __ABS_binding2.getBinding(0),(ABS.StdLib.Set<A>) __ABS_binding2.getBinding(1));
                throw new abs.backend.java.lib.expr.UnmatchedCaseException("abslang.abs:153:2:  value " + __ABS_value + " did not match any pattern.");
            }
        }.of(ss, e, ss));
    }
}
