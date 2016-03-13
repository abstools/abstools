package ABS.Productline;
// abslang.abs:1113:0: 
public final class product_features_f implements abs.backend.java.lib.types.ABSFunction {
    private product_features_f() { }
    public static ABS.StdLib.List<ABS.Productline.Feature> apply() {
        return (new ABS.StdLib.List_Cons(new ABS.Productline.Feature_Program(), new ABS.StdLib.List_Cons(new ABS.Productline.Feature_Type(), new ABS.StdLib.List_Cons(new ABS.Productline.Feature_Source(), new ABS.StdLib.List_Cons(new ABS.Productline.Feature_GuidePage(), new ABS.StdLib.List_Nil())))));
    }
}
