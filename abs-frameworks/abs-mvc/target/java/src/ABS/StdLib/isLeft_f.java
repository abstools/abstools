package ABS.StdLib;
// abslang.abs:124:0: 
public final class isLeft_f implements abs.backend.java.lib.types.ABSFunction {
    private isLeft_f() { }
    public static <A extends abs.backend.java.lib.types.ABSValue,B extends abs.backend.java.lib.types.ABSValue>abs.backend.java.lib.types.ABSBool apply(ABS.StdLib.Either<A,B> val) {
        return (new abs.backend.java.lib.expr.Case() {
            public abs.backend.java.lib.types.ABSBool of(final ABS.StdLib.Either<A,B> val, final ABS.StdLib.Either<A,B> __ABS_value) {
                final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternConstructor(ABS.StdLib.Either_Left.class,new abs.backend.java.lib.expr.PatternVariable("x")).match(__ABS_value);
                if (__ABS_binding0 != null) return new Object() {
                    public abs.backend.java.lib.types.ABSBool execute(final A x) { return abs.backend.java.lib.types.ABSBool.TRUE; }
                }.execute((A) __ABS_binding0.getBinding(0));
                final abs.backend.java.lib.expr.PatternBinding __ABS_binding1 = new abs.backend.java.lib.expr.AnyPattern().match(__ABS_value);
                if (__ABS_binding1 != null) return new Object() {
                    public abs.backend.java.lib.types.ABSBool execute() { return abs.backend.java.lib.types.ABSBool.FALSE; }
                }.execute();
                throw new abs.backend.java.lib.expr.UnmatchedCaseException("abslang.abs:125:4:  value " + __ABS_value + " did not match any pattern.");
            }
        }.of(val, val));
    }
}
