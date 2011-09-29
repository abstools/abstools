/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.typechecker.ext;

import abs.backend.maude.MaudeCompilerHelper;
import abs.frontend.analyser.ErrorMessage;
import abs.frontend.analyser.TypeError;
import abs.frontend.ast.ASTNode;
import abs.frontend.ast.AssignStmt;
import abs.frontend.ast.ExpressionStmt;
import abs.frontend.ast.Model;
import abs.frontend.ast.PureExp;
import abs.frontend.ast.VarDeclStmt;

public class DeadlineChecker extends DefaultTypeSystemExtension {

    protected DeadlineChecker(Model m) {
        super(m);
    }

    @Override
    public void checkExpressionStmt(ExpressionStmt e) {
        checkDeadlineCorrect(e, MaudeCompilerHelper.getAnnotationValue(e.getAnnotations(), "Deadline"));
    }

    @Override
    public void checkAssignStmt(AssignStmt s) {
        checkDeadlineCorrect(s, MaudeCompilerHelper.getAnnotationValue(s.getAnnotations(), "Deadline"));
    }
    
    @Override
    public void checkVarDeclStmt(VarDeclStmt v) {
        checkDeadlineCorrect(v, MaudeCompilerHelper.getAnnotationValue(v.getAnnotations(), "Deadline"));
    }

    private void checkDeadlineCorrect(ASTNode<?> n, PureExp deadline) {
        if (deadline == null) return;
        if (!deadline.getType().getQualifiedName().equals("ABS.StdLib.Duration")) {
            errors.add(new TypeError(n, ErrorMessage.WRONG_DEADLINE_TYPE,
                                     deadline.getType().getQualifiedName()));
        }
    }
}
