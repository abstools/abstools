package org.abs_models.frontend.pardef;

import org.abs_models.frontend.ast.Access;
import org.abs_models.frontend.ast.Annotation;
import org.abs_models.frontend.ast.FnApp;
import org.abs_models.frontend.ast.FunctionDecl;
import org.abs_models.frontend.ast.IntLiteral;
import org.abs_models.frontend.ast.List;
import org.abs_models.frontend.ast.ListLiteral;
import org.abs_models.frontend.ast.PureExp;
import org.abs_models.frontend.ast.Stmt;
import org.abs_models.frontend.ast.TypeUse;
import org.abs_models.frontend.ast.TypedAnnotation;
import org.abs_models.frontend.ast.UnresolvedTypeUse;
import java.util.Objects;

public final class AnnotationUtil {

    // Annotation type names
    private static final String EXPANSION = "Expansion";
    private static final String EXPANSION_CALL = "ExpansionCall";

    private AnnotationUtil() {
    }

    private static TypeUse expansionType() {
        return new UnresolvedTypeUse(EXPANSION, new List<>());
    }

    private static TypeUse expansionCallType() {
        return new UnresolvedTypeUse(EXPANSION_CALL, new List<>());
    }

    public static void annotateExpansion(FunctionDecl expansion, int expansionId) {
        IntLiteral indexLiteral = new IntLiteral(Integer.toString(expansionId));
        Annotation annotation = new TypedAnnotation(indexLiteral, expansionType());
        expansion.addAnnotation(annotation);
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
            addToAnnotations(parentFunction.getAnnotations(), expansionCallType(), expansionId);
        } else {
            addToAnnotations(parent.getAnnotations(), expansionCallType(), expansionId);
        }
    }

    /**
     * Gets the expansion ID of a function declaration. If the function declaration is not an expansion, -1 is
     * returned.
     *
     * @param decl a function declaration
     * @return an expansion ID, or -1
     * @throws NullPointerException if decl is null
     */
    public static int getExpansionId(FunctionDecl decl) {
        Objects.requireNonNull(decl);
        Annotation annotation = getAnnotation(decl.getAnnotationsNoTransform(), expansionType());
        if (annotation == null) {
            return -1;
        }
        PureExp value = annotation.getValue();
        if (value instanceof IntLiteral) {
            IntLiteral intValue = (IntLiteral) value;
            try {
                int result = Integer.parseInt(intValue.getContent());
                return result < 0 ? -1 : result;
            } catch (NumberFormatException e) {
                return -1;
            }
        } else {
            return -1;
        }
    }

    private static Annotation getAnnotation(List<Annotation> annotations, Access annotationType) {
        for (Annotation annotation : annotations) {
            if (annotation instanceof TypedAnnotation) {
                TypedAnnotation typedAnnotation = (TypedAnnotation) annotation;
                if (typedAnnotation.getAccess().matches(annotationType)) {
                    return annotation;
                }
            }
        }
        return null;
    }

    /**
     * Add an ExpansionCall annotation, or an argument to an existing
     * annotation.  Creates or adds to [ExpansionCall : list[expansionId]]
     * annotation.
     *
     * @param annotations The list to mutate
     * @param annotationType Currently always EXPANSION_CALL
     * @param expansionId An integer to add to the list.
     */
    private static void addToAnnotations(List<Annotation> annotations,
                                         Access annotationType,
                                         int expansionId)
    {
        IntLiteral indexLiteral = new IntLiteral(Integer.toString(expansionId));
        Annotation toAdd = getAnnotation(annotations, annotationType);

        if (toAdd == null) {
            List<PureExp> llist = new List<>(new ListLiteral(new List<PureExp>(indexLiteral)));
            toAdd = new TypedAnnotation(new FnApp("list", llist), annotationType);
            annotations.add(toAdd);
        } else {
            PureExp value = toAdd.getValue();
            if (!(value instanceof FnApp)) {
                throw new IllegalArgumentException("Annotation list contains invalid expansion annotation");
            }
            FnApp fvalue = (FnApp)value;
            if (!fvalue.getName().equals("list")) {
                throw new IllegalArgumentException("Annotation list contains invalid expansion annotation");
            }
            ListLiteral list = (ListLiteral) fvalue.getParam(0);
            for (PureExp exp : list.getPureExps()) {
                if (exp instanceof IntLiteral) {
                    IntLiteral intLiteral = (IntLiteral) exp;
                    if (intLiteral.getContent().equals(indexLiteral.getContent())) {
                        return;
                    }
                }
            }
            list.addPureExp(indexLiteral);
        }
    }
}
