package ABS.StdLib;
// abslang.abs:254:0: 
public final class hasNext_f implements abs.backend.java.lib.types.ABSFunction {
    private hasNext_f() { }
    public static <A extends abs.backend.java.lib.types.ABSValue>abs.backend.java.lib.types.ABSBool apply(ABS.StdLib.Set<A> s) {
        return (ABS.StdLib.emptySet_f.apply(s).negate());
    }
}
