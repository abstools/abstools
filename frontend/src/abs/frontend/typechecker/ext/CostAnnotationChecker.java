/**
 * Copyright (c) 2016, The Envisage Project. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.typechecker.ext;

import abs.common.CompilerUtils;
import abs.frontend.analyser.ErrorMessage;
import abs.frontend.analyser.TypeError;
import abs.frontend.ast.ASTNode;
import abs.frontend.ast.AssignStmt;
import abs.frontend.ast.ExpressionStmt;
import abs.frontend.ast.Model;
import abs.frontend.ast.PureExp;
import abs.frontend.ast.Stmt;

public class CostAnnotationChecker extends DefaultTypeSystemExtension {

    protected CostAnnotationChecker(Model m) {
        super(m);
    }

    @Override
    public void checkStmt(Stmt s) {
        PureExp cost = CompilerUtils.getAnnotationValueFromName(s.getAnnotations(), "ABS.DC.Cost");
        if (cost == null) return;
        if (!cost.getType().isNumericType()) {
            errors.add(new TypeError(s, ErrorMessage.WRONG_COST_ANNOTATION_TYPE,
                                     cost.getType().getQualifiedName()));
        }
    }
}
