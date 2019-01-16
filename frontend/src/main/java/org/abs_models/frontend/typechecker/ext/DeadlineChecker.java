/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.frontend.typechecker.ext;

import org.abs_models.common.CompilerUtils;
import org.abs_models.frontend.analyser.ErrorMessage;
import org.abs_models.frontend.analyser.TypeError;
import org.abs_models.frontend.ast.ASTNode;
import org.abs_models.frontend.ast.AssignStmt;
import org.abs_models.frontend.ast.ExpressionStmt;
import org.abs_models.frontend.ast.Model;
import org.abs_models.frontend.ast.PureExp;
import org.abs_models.frontend.ast.VarDeclStmt;

public class DeadlineChecker extends DefaultTypeSystemExtension {

    protected DeadlineChecker(Model m) {
        super(m);
    }

    @Override
    public void checkExpressionStmt(ExpressionStmt e) {
        checkDeadlineCorrect(e, CompilerUtils.getAnnotationValueFromName(e.getAnnotations(), "ABS.StdLib.Deadline"));
    }

    @Override
    public void checkAssignStmt(AssignStmt s) {
        checkDeadlineCorrect(s, CompilerUtils.getAnnotationValueFromName(s.getAnnotations(), "ABS.StdLib.Deadline"));
    }

    @Override
    public void checkVarDeclStmt(VarDeclStmt v) {
        checkDeadlineCorrect(v, CompilerUtils.getAnnotationValueFromName(v.getAnnotations(), "ABS.StdLib.Deadline"));
    }

    private void checkDeadlineCorrect(ASTNode<?> n, PureExp deadline) {
        if (deadline == null) return;
        deadline.typeCheck(errors);
        if (deadline.getType().isUnknownType()
            || !deadline.getType().getQualifiedName().equals("ABS.StdLib.Duration")) {
            errors.add(new TypeError(n, ErrorMessage.WRONG_DEADLINE_TYPE,
                                     deadline.getType().getQualifiedName()));
        }
    }
}
