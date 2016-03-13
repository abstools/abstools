package ABS.StdLib;
// abslang.abs:162:0: 
public final class emptySet_f implements abs.backend.java.lib.types.ABSFunction {
    private emptySet_f() { }
    public static <A extends abs.backend.java.lib.types.ABSValue>abs.backend.java.lib.types.ABSBool apply(ABS.StdLib.Set<A> xs) {
        return (abs.backend.java.lib.expr.BinOp.eq(xs,new ABS.StdLib.Set_EmptySet()));
    }
}
