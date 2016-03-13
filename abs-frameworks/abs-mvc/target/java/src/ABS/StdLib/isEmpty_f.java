package ABS.StdLib;
// abslang.abs:279:0: 
public final class isEmpty_f implements abs.backend.java.lib.types.ABSFunction {
    private isEmpty_f() { }
    public static <A extends abs.backend.java.lib.types.ABSValue>abs.backend.java.lib.types.ABSBool apply(ABS.StdLib.List<A> list) {
        return (abs.backend.java.lib.expr.BinOp.eq(list,new ABS.StdLib.List_Nil()));
    }
}
