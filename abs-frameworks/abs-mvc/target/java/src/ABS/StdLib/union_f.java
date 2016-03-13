package ABS.StdLib;
// abslang.abs:176:0: 
public final class union_f implements abs.backend.java.lib.types.ABSFunction {
    private union_f() { }
    public static <A extends abs.backend.java.lib.types.ABSValue>ABS.StdLib.Set<A> apply(ABS.StdLib.Set<A> set1, ABS.StdLib.Set<A> set2) {
        return (new abs.backend.java.lib.expr.Case() {
            public ABS.StdLib.Set<A> of(final ABS.StdLib.Set<A> set2, final ABS.StdLib.Set<A> set1, final ABS.StdLib.Set<A> __ABS_value) {
                final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternConstructor(ABS.StdLib.Set_EmptySet.class).match(__ABS_value);
                if (__ABS_binding0 != null) return new Object() {
                    public ABS.StdLib.Set<A> execute() { return set2; }
                }.execute();
                final abs.backend.java.lib.expr.PatternBinding __ABS_binding1 = new abs.backend.java.lib.expr.PatternConstructor(ABS.StdLib.Set_Insert.class,new abs.backend.java.lib.expr.PatternVariable("e1"),new abs.backend.java.lib.expr.PatternVariable("ss1")).match(__ABS_value);
                if (__ABS_binding1 != null) return new Object() {
                    public ABS.StdLib.Set<A> execute(final A e1, final ABS.StdLib.Set<A> ss1) { return new abs.backend.java.lib.expr.Case() {
                        public ABS.StdLib.Set<A> of(final ABS.StdLib.Set<A> ss1, final ABS.StdLib.Set<A> set2, final ABS.StdLib.Set<A> set1, final A e1, final ABS.StdLib.Set<A> __ABS_value) {
                            final abs.backend.java.lib.expr.PatternBinding __ABS_binding0 = new abs.backend.java.lib.expr.PatternConstructor(ABS.StdLib.Set_EmptySet.class).match(__ABS_value);
                            if (__ABS_binding0 != null) return new Object() {
                                public ABS.StdLib.Set<A> execute() { return set1; }
                            }.execute();
                            final abs.backend.java.lib.expr.PatternBinding __ABS_binding1 = new abs.backend.java.lib.expr.PatternConstructor(ABS.StdLib.Set_Insert.class,new abs.backend.java.lib.expr.PatternValue(e1),new abs.backend.java.lib.expr.PatternVariable("ss2")).match(__ABS_value);
                            if (__ABS_binding1 != null) return new Object() {
                                public ABS.StdLib.Set<A> execute(final ABS.StdLib.Set<A> ss2) { return new ABS.StdLib.Set_Insert(e1, ABS.StdLib.union_f.apply(ss1, ss2)); }
                            }.execute((ABS.StdLib.Set<A>) __ABS_binding1.getBinding(0));
                            final abs.backend.java.lib.expr.PatternBinding __ABS_binding2 = new abs.backend.java.lib.expr.PatternConstructor(ABS.StdLib.Set_Insert.class,new abs.backend.java.lib.expr.PatternVariable("e2"),new abs.backend.java.lib.expr.PatternVariable("ss2")).match(__ABS_value);
                            if (__ABS_binding2 != null) return new Object() {
                                public ABS.StdLib.Set<A> execute(final A e2, final ABS.StdLib.Set<A> ss2) { return abs.backend.java.lib.expr.BinOp.lt(e1,e2).toBoolean() ? new ABS.StdLib.Set_Insert(e1, ABS.StdLib.union_f.apply(ss1, set2)) : new ABS.StdLib.Set_Insert(e2, ABS.StdLib.union_f.apply(set1, ss2)); }
                            }.execute((A) __ABS_binding2.getBinding(0),(ABS.StdLib.Set<A>) __ABS_binding2.getBinding(1));
                            throw new abs.backend.java.lib.expr.UnmatchedCaseException("abslang.abs:179:26:  value " + __ABS_value + " did not match any pattern.");
                        }
                    }.of(ss1, set2, set1, e1, set2); }
                }.execute((A) __ABS_binding1.getBinding(0),(ABS.StdLib.Set<A>) __ABS_binding1.getBinding(1));
                throw new abs.backend.java.lib.expr.UnmatchedCaseException("abslang.abs:177:3:  value " + __ABS_value + " did not match any pattern.");
            }
        }.of(set2, set1, set1));
    }
}
