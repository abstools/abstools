/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.common;


import org.abs_models.frontend.ast.ASTNode;
import org.abs_models.frontend.ast.Exp;
import org.abs_models.frontend.ast.Stmt;

public class CompilerUtils {

    /**
     * Find the enclosing Stmt of an expression.
     * TODO: throw error when not found
     */
    public static Stmt findStmtForExpression(Exp e) {
        ASTNode<?> node = e;
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
