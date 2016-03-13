package ABS.StdLib;
// abslang.abs:328:0: 
public final class copy_f implements abs.backend.java.lib.types.ABSFunction {
    private copy_f() { }
    public static <A extends abs.backend.java.lib.types.ABSValue>ABS.StdLib.List<A> apply(A p, abs.backend.java.lib.types.ABSInteger n) {
        return (new abs.backend.java.lib.expr.Case() {
            public ABS.StdLib.List<A> of(final A p, final abs.backend.java.lib.types.ABSInteger n, final abs.backend.java.lib.types.ABSInteger __ABS_value) {
                final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternValue(abs.backend.java.lib.types.ABSInteger.fromString("0")).match(__ABS_value);
                if (__ABS_binding0 != null) return new Object() {
                    public ABS.StdLib.List<A> execute() { return new ABS.StdLib.List_Nil(); }
                }.execute();
                final abs.backend.java.lib.expr.PatternBinding __ABS_binding1 = new abs.backend.java.lib.expr.PatternVariable("m").match(__ABS_value);
                if (__ABS_binding1 != null) return new Object() {
                    public ABS.StdLib.List<A> execute(final abs.backend.java.lib.types.ABSInteger m) { return new ABS.StdLib.List_Cons(p, ABS.StdLib.copy_f.apply(p, m.subtract(abs.backend.java.lib.types.ABSInteger.fromString("1")))); }
                }.execute((abs.backend.java.lib.types.ABSInteger) __ABS_binding1.getBinding(0));
                throw new abs.backend.java.lib.expr.UnmatchedCaseException("abslang.abs:329:3:  value " + __ABS_value + " did not match any pattern.");
            }
        }.of(p, n, n));
    }
}
