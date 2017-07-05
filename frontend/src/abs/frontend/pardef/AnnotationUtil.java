package abs.frontend.pardef;

import abs.frontend.ast.Access;
import abs.frontend.ast.Annotation;
import abs.frontend.ast.FnApp;
import abs.frontend.ast.FunctionDecl;
import abs.frontend.ast.IntLiteral;
import abs.frontend.ast.List;
import abs.frontend.ast.ListLiteral;
import abs.frontend.ast.PureExp;
import abs.frontend.ast.Stmt;
import abs.frontend.ast.TypedAnnotation;

public final class AnnotationUtil {

    private AnnotationUtil() {
    }

    public static void annotateExpansion(FunctionDecl expansion, int expansionIndex) {
        addToAnnotations(expansion.getAnnotations(), TypeUtil.expansionType(), expansionIndex);
    }

    public static void annotateCall(FnApp call, int expansionIndex) {
        Stmt parent = TreeUtil.findParent(call, Stmt.class);
        if (parent == null) {
            FunctionDecl parentFunction = TreeUtil.findParent(call, FunctionDecl.class);
            if (parentFunction != null) {
                addToAnnotations(parentFunction.getAnnotations(), TypeUtil.expansionCallType(), expansionIndex);
            }
            // TODO handle missing parent statement
        } else {
            addToAnnotations(parent.getAnnotations(), TypeUtil.expansionCallType(), expansionIndex);
        }
    }

    private static void addToAnnotations(List<Annotation> annotations, Access annotationType, int expansionIndex) {
        PureExp indexLiteral = new IntLiteral(Integer.toString(expansionIndex));
        Annotation toAdd = null;
        for (Annotation annotation : annotations) {
            if (annotation instanceof TypedAnnotation) {
                TypedAnnotation typedAnnotation = (TypedAnnotation) annotation;
                if (typedAnnotation.getAccess().matches(annotationType)) {
                    toAdd = annotation;
                    break;
                }
            }
        }

        if (toAdd == null) {
            toAdd = new TypedAnnotation(new ListLiteral(new List<PureExp>()), annotationType);
            annotations.add(toAdd);
        }

        PureExp value = toAdd.getValue();
        if (value instanceof ListLiteral) {
            ListLiteral list = (ListLiteral) value;
            list.addPureExp(indexLiteral);
        } else {
            throw new IllegalArgumentException("Annotation list contains invalid expansion annotation");
        }
    }
}
