/** 
 * Copyright (c) 2009-2011, The ENVISAGE Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.frontend.delta;

import org.abs_models.frontend.ast.ASTNode;
import org.abs_models.frontend.ast.DeltaDecl;

public class DeltaModellingWithNodeException extends DeltaModellingException {

    private static final long serialVersionUID = 1L;
    private final ASTNode<?> node;

    public DeltaModellingWithNodeException(ASTNode<?> node, String msg) {
        this(node, null, msg);
    }

    public DeltaModellingWithNodeException(ASTNode<?> node, DeltaDecl d, String msg) {
        super(d, msg);
        this.node = node;
    }

    public ASTNode<?> getNode() { return node; }
}
