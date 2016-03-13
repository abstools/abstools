package ABS.StdLib;
// abslang.abs:386:0: 
public final class lookupUnsafe_f implements abs.backend.java.lib.types.ABSFunction {
    private lookupUnsafe_f() { }
    public static <A extends abs.backend.java.lib.types.ABSValue,B extends abs.backend.java.lib.types.ABSValue>B apply(ABS.StdLib.Map<A,B> ms, A k) {
        return (ABS.StdLib.fromJust_f.apply(ABS.StdLib.lookup_f.apply(ms, k)));
    }
}
