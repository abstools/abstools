package ABS.DC;
// abslang.abs:629:0: 
public final class sumDivsN_f implements abs.backend.java.lib.types.ABSFunction {
    private sumDivsN_f() { }
    public static abs.backend.java.lib.types.ABSRational apply(ABS.StdLib.List<abs.backend.java.lib.types.ABSInteger> consumeds, ABS.StdLib.List<abs.backend.java.lib.types.ABSInteger> totals, abs.backend.java.lib.types.ABSInteger n) {
        return (abs.backend.java.lib.expr.BinOp.eq(n,abs.backend.java.lib.types.ABSInteger.fromString("0")).or(ABS.StdLib.isEmpty_f.apply(consumeds)).or(ABS.StdLib.isEmpty_f.apply(totals)).toBoolean() ? abs.backend.java.lib.types.ABSInteger.fromString("0") : ABS.StdLib.head_f.apply(consumeds).multiply(abs.backend.java.lib.types.ABSInteger.fromString("100")).divide(ABS.StdLib.head_f.apply(totals)).add(ABS.DC.sumDivsN_f.apply(ABS.StdLib.tail_f.apply(consumeds), ABS.StdLib.tail_f.apply(totals), n.subtract(abs.backend.java.lib.types.ABSInteger.fromString("1")))));
    }
}
