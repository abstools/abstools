package ABS.StdLib;
// abslang.abs:0:0: 
public final class right_f implements abs.backend.java.lib.types.ABSFunction {
    private right_f() { }
    public static <A extends abs.backend.java.lib.types.ABSValue,B extends abs.backend.java.lib.types.ABSValue>B apply(ABS.StdLib.Either<A,B> data) {
        return (new abs.backend.java.lib.expr.Case() {
            public B of(final ABS.StdLib.Either<A,B> data, final ABS.StdLib.Either<A,B> __ABS_value) {
                final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternConstructor(ABS.StdLib.Either_Right.class,new abs.backend.java.lib.expr.PatternVariable("res")).match(__ABS_value);
                if (__ABS_binding0 != null) return new Object() {
                    public B execute(final B res) { return res; }
                }.execute((B) __ABS_binding0.getBinding(0));
                throw new abs.backend.java.lib.expr.UnmatchedCaseException("abslang.abs:0:0:  value " + __ABS_value + " did not match any pattern.");
            }
        }.of(data, data));
    }
}
