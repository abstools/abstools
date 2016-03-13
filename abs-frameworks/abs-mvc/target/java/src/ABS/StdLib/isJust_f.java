package ABS.StdLib;
// abslang.abs:119:0: 
public final class isJust_f implements abs.backend.java.lib.types.ABSFunction {
    private isJust_f() { }
    public static <A extends abs.backend.java.lib.types.ABSValue>abs.backend.java.lib.types.ABSBool apply(ABS.StdLib.Maybe<A> a) {
        return (new abs.backend.java.lib.expr.Case() {
            public abs.backend.java.lib.types.ABSBool of(final ABS.StdLib.Maybe<A> a, final ABS.StdLib.Maybe<A> __ABS_value) {
                final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternConstructor(ABS.StdLib.Maybe_Just.class,new abs.backend.java.lib.expr.PatternVariable("j")).match(__ABS_value);
                if (__ABS_binding0 != null) return new Object() {
                    public abs.backend.java.lib.types.ABSBool execute(final A j) { return abs.backend.java.lib.types.ABSBool.TRUE; }
                }.execute((A) __ABS_binding0.getBinding(0));
                final abs.backend.java.lib.expr.PatternBinding __ABS_binding1 = new abs.backend.java.lib.expr.PatternConstructor(ABS.StdLib.Maybe_Nothing.class).match(__ABS_value);
                if (__ABS_binding1 != null) return new Object() {
                    public abs.backend.java.lib.types.ABSBool execute() { return abs.backend.java.lib.types.ABSBool.FALSE; }
                }.execute();
                throw new abs.backend.java.lib.expr.UnmatchedCaseException("abslang.abs:120:4:  value " + __ABS_value + " did not match any pattern.");
            }
        }.of(a, a));
    }
}
