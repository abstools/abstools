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

public class DeploymentComponentChecker extends DefaultTypeSystemExtension {

    protected DeploymentComponentChecker(Model m) {
        super(m);
    }

    @Override
    public void checkExpressionStmt(ExpressionStmt expressionStmt) {
        checkDCCorrect(expressionStmt, MaudeCompilerHelper.getAnnotationValue(expressionStmt.getAnnotations(), "DC"));
    }

    @Override
    public void checkAssignStmt(AssignStmt s) {
        checkDCCorrect(s, MaudeCompilerHelper.getAnnotationValue(s.getAnnotations(), "DC"));
    }

    @Override
    public void checkVarDeclStmt(VarDeclStmt varDeclStmt) {
        checkDCCorrect(varDeclStmt, MaudeCompilerHelper.getAnnotationValue(varDeclStmt.getAnnotations(), "DC"));
    }

    private void checkDCCorrect(ASTNode<?> n, PureExp dc) {
        if (dc == null) return;
        if (!dc.getType().getQualifiedName().equals("ABS.DC.DeploymentComponent") && !dc.getType().getQualifiedName().equals("ABS.DC.DC")) {
            errors.add(new TypeError(n, ErrorMessage.WRONG_DEPLOYMENT_COMPONENT, dc.getType().getQualifiedName()));
        }
    }
}
