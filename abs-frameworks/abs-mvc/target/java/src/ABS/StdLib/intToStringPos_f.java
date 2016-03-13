package ABS.StdLib;
// abslang.abs:426:0: 
public final class intToStringPos_f implements abs.backend.java.lib.types.ABSFunction {
    private intToStringPos_f() { }
    public static abs.backend.java.lib.types.ABSString apply(abs.backend.java.lib.types.ABSInteger n) {
        return (new abs.backend.java.lib.expr.Let() { public abs.backend.java.lib.types.ABSString in(final abs.backend.java.lib.types.ABSInteger n, final abs.backend.java.lib.types.ABSInteger div) { return new abs.backend.java.lib.expr.Let() { public abs.backend.java.lib.types.ABSString in(final abs.backend.java.lib.types.ABSInteger div, final abs.backend.java.lib.types.ABSInteger n, final abs.backend.java.lib.types.ABSInteger res) { return new abs.backend.java.lib.expr.Case() {
            public abs.backend.java.lib.types.ABSString of(final abs.backend.java.lib.types.ABSInteger div, final abs.backend.java.lib.types.ABSInteger res, final abs.backend.java.lib.types.ABSInteger n, final abs.backend.java.lib.types.ABSInteger __ABS_value) {
                final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternValue(abs.backend.java.lib.types.ABSInteger.fromString("0")).match(__ABS_value);
                if (__ABS_binding0 != null) return new Object() {
                    public abs.backend.java.lib.types.ABSString execute() { return abs.backend.java.lib.types.ABSString.fromString("0"); }
                }.execute();
                final abs.backend.java.lib.expr.PatternBinding __ABS_binding1 = new abs.backend.java.lib.expr.PatternValue(abs.backend.java.lib.types.ABSInteger.fromString("1")).match(__ABS_value);
                if (__ABS_binding1 != null) return new Object() {
                    public abs.backend.java.lib.types.ABSString execute() { return abs.backend.java.lib.types.ABSString.fromString("1"); }
                }.execute();
                final abs.backend.java.lib.expr.PatternBinding __ABS_binding2 = new abs.backend.java.lib.expr.PatternValue(abs.backend.java.lib.types.ABSInteger.fromString("2")).match(__ABS_value);
                if (__ABS_binding2 != null) return new Object() {
                    public abs.backend.java.lib.types.ABSString execute() { return abs.backend.java.lib.types.ABSString.fromString("2"); }
                }.execute();
                final abs.backend.java.lib.expr.PatternBinding __ABS_binding3 = new abs.backend.java.lib.expr.PatternValue(abs.backend.java.lib.types.ABSInteger.fromString("3")).match(__ABS_value);
                if (__ABS_binding3 != null) return new Object() {
                    public abs.backend.java.lib.types.ABSString execute() { return abs.backend.java.lib.types.ABSString.fromString("3"); }
                }.execute();
                final abs.backend.java.lib.expr.PatternBinding __ABS_binding4 = new abs.backend.java.lib.expr.PatternValue(abs.backend.java.lib.types.ABSInteger.fromString("4")).match(__ABS_value);
                if (__ABS_binding4 != null) return new Object() {
                    public abs.backend.java.lib.types.ABSString execute() { return abs.backend.java.lib.types.ABSString.fromString("4"); }
                }.execute();
                final abs.backend.java.lib.expr.PatternBinding __ABS_binding5 = new abs.backend.java.lib.expr.PatternValue(abs.backend.java.lib.types.ABSInteger.fromString("5")).match(__ABS_value);
                if (__ABS_binding5 != null) return new Object() {
                    public abs.backend.java.lib.types.ABSString execute() { return abs.backend.java.lib.types.ABSString.fromString("5"); }
                }.execute();
                final abs.backend.java.lib.expr.PatternBinding __ABS_binding6 = new abs.backend.java.lib.expr.PatternValue(abs.backend.java.lib.types.ABSInteger.fromString("6")).match(__ABS_value);
                if (__ABS_binding6 != null) return new Object() {
                    public abs.backend.java.lib.types.ABSString execute() { return abs.backend.java.lib.types.ABSString.fromString("6"); }
                }.execute();
                final abs.backend.java.lib.expr.PatternBinding __ABS_binding7 = new abs.backend.java.lib.expr.PatternValue(abs.backend.java.lib.types.ABSInteger.fromString("7")).match(__ABS_value);
                if (__ABS_binding7 != null) return new Object() {
                    public abs.backend.java.lib.types.ABSString execute() { return abs.backend.java.lib.types.ABSString.fromString("7"); }
                }.execute();
                final abs.backend.java.lib.expr.PatternBinding __ABS_binding8 = new abs.backend.java.lib.expr.PatternValue(abs.backend.java.lib.types.ABSInteger.fromString("8")).match(__ABS_value);
                if (__ABS_binding8 != null) return new Object() {
                    public abs.backend.java.lib.types.ABSString execute() { return abs.backend.java.lib.types.ABSString.fromString("8"); }
                }.execute();
                final abs.backend.java.lib.expr.PatternBinding __ABS_binding9 = new abs.backend.java.lib.expr.PatternValue(abs.backend.java.lib.types.ABSInteger.fromString("9")).match(__ABS_value);
                if (__ABS_binding9 != null) return new Object() {
                    public abs.backend.java.lib.types.ABSString execute() { return abs.backend.java.lib.types.ABSString.fromString("9"); }
                }.execute();
                final abs.backend.java.lib.expr.PatternBinding __ABS_binding10 = new abs.backend.java.lib.expr.AnyPattern().match(__ABS_value);
                if (__ABS_binding10 != null) return new Object() {
                    public abs.backend.java.lib.types.ABSString execute() { return ABS.StdLib.intToStringPos_f.apply(div).add(ABS.StdLib.intToStringPos_f.apply(res)); }
                }.execute();
                throw new abs.backend.java.lib.expr.UnmatchedCaseException("abslang.abs:429:2:  value " + __ABS_value + " did not match any pattern.");
            }
        }.of(div, res, n, n); }}.in(div, n, n.mod(abs.backend.java.lib.types.ABSInteger.fromString("10"))); }}.in(n, n.divide(abs.backend.java.lib.types.ABSInteger.fromString("10")).truncate()));
    }
}
