/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.common;

import abs.frontend.ast.ASTNode;
import abs.frontend.ast.Annotation;
import abs.frontend.ast.DataConstructorExp;
import abs.frontend.ast.Exp;
import abs.frontend.ast.List;
import abs.frontend.ast.PureExp;
import abs.frontend.ast.Stmt;
import abs.frontend.ast.TypeUse;
import abs.frontend.ast.TypedAnnotation;

public class CompilerUtils {

    /**
     * Get the value of an annotation.  Will return the value of the first
     * annotation with the given simple name.
     *
     * @param annotationName The simple name (without module prefix) of the
     * annotation
     * @return a <code>PureExp</code> value or null
     */
    public static PureExp getAnnotationValueFromSimpleName(List<Annotation> annotations, String annotationName) {
        for (Annotation a : annotations) {
            if (a instanceof TypedAnnotation) {
                TypedAnnotation ta = (TypedAnnotation)a;
                if (((TypeUse)ta.getAccess()).getName().equals(annotationName))
                    return ta.getValue();
            }
        }
        return null;
    }

    /**
     * Get the value of an annotation.  Returns the value of the first
     * annotation with the given qualified (module-prefixed) name.
     *
     * @param annotationName The simple name (without module prefix) of the
     * annotation
     * @return a <code>PureExp</code> value or null
     */
    public static PureExp getAnnotationValueFromName(List<Annotation> annotations, String qualifiedAnnotationName) {
        for (Annotation a : annotations) {
            if (a instanceof TypedAnnotation) {
                TypedAnnotation ta = (TypedAnnotation)a;
                if (((TypeUse)ta.getAccess()).getDecl().qualifiedName().equals(qualifiedAnnotationName))
                    return ta.getValue();
            }
        }
        return null;
    }

    /**
     * Find the enclosing Stmt of an expression.
     * TODO: throw error when not found
     */
    public static Stmt findStmtForExpression(Exp e) {
        ASTNode node = e;
        while (node != null && !(node instanceof Stmt)) {
            node = node.getParent();
        }
        return (Stmt)node;
    }

    /**
     * Copies the position of fromNode to toNode.
     *
     * @param fromNode
     * @param toNode
     * @return toNode
     */
    @SuppressWarnings("rawtypes")
    public static <T extends ASTNode, U extends ASTNode> U copyPosition(T fromNode, U toNode) {
        toNode.setPositionFromNode(fromNode);
        return toNode;
    }

}
