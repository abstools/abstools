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

    public static void annotateExpansion(FunctionDecl expansion, int expansionId) {
        addToAnnotations(expansion.getAnnotations(), TypeUtil.expansionType(), expansionId);
    }

    /**
     * <p>Annotates the parent Statement or Function node with an ExpansionCall annotation.</p>
     *
     * <p>If said node already has an annotation with the ExpansionCall type, the expansionIndex will be added to the
     * existing annotation.</p>
     *
     * @param call the call to use as a starting point to look for a Stmt or FunctionDecl parent
     * @param expansionId the ID of the called Expansion
     * @throws IllegalArgumentException if there is no Stmt or FunctionDecl parent
     */
    public static void annotateCall(FnApp call, int expansionId) {
        Stmt parent = call.closestParent(Stmt.class);
        if (parent == null) {
            FunctionDecl parentFunction = call.closestParent(FunctionDecl.class);
            if (parentFunction == null) {
                throw new IllegalArgumentException("Function call has no parent Statement or FunctionDecl: " + call);
            }
            addToAnnotations(parentFunction.getAnnotations(), TypeUtil.expansionCallType(), expansionId);
        } else {
            addToAnnotations(parent.getAnnotations(), TypeUtil.expansionCallType(), expansionId);
        }
    }

    private static void addToAnnotations(List<Annotation> annotations, Access annotationType, int expansionId) {
        IntLiteral indexLiteral = new IntLiteral(Integer.toString(expansionId));
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
            for (PureExp exp : list.getPureExps()) {
                if (exp instanceof IntLiteral) {
                    IntLiteral intLiteral = (IntLiteral) exp;
                    if (intLiteral.getContent().equals(indexLiteral.getContent())) {
                        return;
                    }
                }
            }
            list.addPureExp(indexLiteral);
        } else {
            throw new IllegalArgumentException("Annotation list contains invalid expansion annotation");
        }
    }
}
