package ABS.StdLib;
// abslang.abs:380:0: 
public final class lookupMaybe_f implements abs.backend.java.lib.types.ABSFunction {
    private lookupMaybe_f() { }
    public static <A extends abs.backend.java.lib.types.ABSValue,B extends abs.backend.java.lib.types.ABSValue>ABS.StdLib.Maybe<B> apply(ABS.StdLib.Map<A,B> ms, A k) {
        return (ABS.StdLib.lookup_f.apply(ms, k));
    }
}
