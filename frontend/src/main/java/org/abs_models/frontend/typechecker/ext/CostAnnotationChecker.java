/**
 * Copyright (c) 2016, The Envisage Project. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.frontend.typechecker.ext;

import org.abs_models.common.CompilerUtils;
import org.abs_models.frontend.analyser.ErrorMessage;
import org.abs_models.frontend.analyser.TypeError;
import org.abs_models.frontend.ast.Model;
import org.abs_models.frontend.ast.PureExp;
import org.abs_models.frontend.ast.Stmt;

public class CostAnnotationChecker extends DefaultTypeSystemExtension {

    protected CostAnnotationChecker(Model m) {
        super(m);
    }

    @Override
    public void checkStmt(Stmt s) {
        PureExp cost = CompilerUtils.getAnnotationValueFromName(s.getAnnotations(), "ABS.DC.Cost");
        if (cost == null) return;
        cost.typeCheck(errors);
        if (!cost.getType().isNumericType()) {
            errors.add(new TypeError(s, ErrorMessage.WRONG_COST_ANNOTATION_TYPE,
                                     cost.getType().getQualifiedName()));
        }
    }
}
