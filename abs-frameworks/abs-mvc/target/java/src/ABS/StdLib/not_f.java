package ABS.StdLib;
// abslang.abs:74:0: 
public final class not_f implements abs.backend.java.lib.types.ABSFunction {
    private not_f() { }
    public static abs.backend.java.lib.types.ABSBool apply(abs.backend.java.lib.types.ABSBool a) {
        return (a.negate());
    }
}
