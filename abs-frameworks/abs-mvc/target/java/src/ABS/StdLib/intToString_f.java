package ABS.StdLib;
// abslang.abs:420:0: 
public final class intToString_f implements abs.backend.java.lib.types.ABSFunction {
    private intToString_f() { }
    public static abs.backend.java.lib.types.ABSString apply(abs.backend.java.lib.types.ABSInteger n) {
        return (new abs.backend.java.lib.expr.Case() {
            public abs.backend.java.lib.types.ABSString of(final abs.backend.java.lib.types.ABSInteger n, final abs.backend.java.lib.types.ABSBool __ABS_value) {
                final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternConstructor(ABS.StdLib.Bool_True.class).match(__ABS_value);
                if (__ABS_binding0 != null) return new Object() {
                    public abs.backend.java.lib.types.ABSString execute() { return abs.backend.java.lib.types.ABSString.fromString("-").add(ABS.StdLib.intToStringPos_f.apply(n.negate())); }
                }.execute();
                final abs.backend.java.lib.expr.PatternBinding __ABS_binding1 = new abs.backend.java.lib.expr.PatternConstructor(ABS.StdLib.Bool_False.class).match(__ABS_value);
                if (__ABS_binding1 != null) return new Object() {
                    public abs.backend.java.lib.types.ABSString execute() { return ABS.StdLib.intToStringPos_f.apply(n); }
                }.execute();
                throw new abs.backend.java.lib.expr.UnmatchedCaseException("abslang.abs:421:2:  value " + __ABS_value + " did not match any pattern.");
            }
        }.of(n, abs.backend.java.lib.expr.BinOp.lt(n,abs.backend.java.lib.types.ABSInteger.fromString("0"))));
    }
}
