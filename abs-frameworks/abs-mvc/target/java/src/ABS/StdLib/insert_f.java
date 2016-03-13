package ABS.StdLib;
// abslang.abs:404:0: 
public final class insert_f implements abs.backend.java.lib.types.ABSFunction {
    private insert_f() { }
    public static <A extends abs.backend.java.lib.types.ABSValue,B extends abs.backend.java.lib.types.ABSValue>ABS.StdLib.Map<A,B> apply(ABS.StdLib.Map<A,B> map, ABS.StdLib.Pair<A,B> p) {
        return (new ABS.StdLib.Map_InsertAssoc(p, map));
    }
}
