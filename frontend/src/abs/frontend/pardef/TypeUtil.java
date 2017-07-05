package abs.frontend.pardef;

import abs.frontend.ast.Annotation;
import abs.frontend.ast.List;
import abs.frontend.ast.ParametricDataTypeUse;
import abs.frontend.ast.TypeUse;
import abs.frontend.ast.UnresolvedTypeUse;

public final class TypeUtil {

    private static final String INT_NAME = "Int";
    private static final String LIST_NAME = "List";
    private static final String EXPANSION = "Expansion";
    private static final String EXPANSION_CALL = "ExpansionCall";

    private TypeUtil() {
    }

    public static TypeUse intType() {
        return type(INT_NAME);
    }

    public static TypeUse intType(List<Annotation> annotations) {
        return type(INT_NAME, annotations);
    }

    public static TypeUse listType(String typeParameter) {
        return listType(type(typeParameter));
    }

    public static TypeUse listType(String typeParameter, List<Annotation> annotations) {
        return listType(type(typeParameter), annotations);
    }

    public static TypeUse listType(TypeUse typeParameter) {
        return listType(typeParameter, new List<Annotation>());
    }

    public static TypeUse listType(TypeUse typeParameter, List<Annotation> annotations) {
        return parametricType(LIST_NAME, new List<>(typeParameter), annotations);
    }

    public static TypeUse expansionType() {
        return type(EXPANSION);
    }

    public static TypeUse expansionCallType() {
        return type(EXPANSION_CALL);
    }


    public static TypeUse type(String name) {
        return type(name, new List<Annotation>());
    }

    public static TypeUse type(String name, List<Annotation> annotations) {
        return new UnresolvedTypeUse(name, annotations);
    }

    public static TypeUse parametricType(String name, TypeUse typeParameter) {
        return parametricType(name, new List<>(typeParameter), new List<Annotation>());
    }

    public static TypeUse parametricType(String name, List<TypeUse> typeParameters) {
        return parametricType(name, typeParameters, new List<Annotation>());
    }

    public static TypeUse parametricType(String name, TypeUse typeParameter, List<Annotation> annotations) {
        return parametricType(name, new List<>(typeParameter), annotations);
    }

    public static TypeUse parametricType(String name, List<TypeUse> typeParameters, List<Annotation> annotations) {
        return new ParametricDataTypeUse(name, annotations, typeParameters);
    }
}
