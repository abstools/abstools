/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.frontend.typechecker.ext;

import org.abs_models.common.CompilerUtils;
import org.abs_models.frontend.analyser.ErrorMessage;
import org.abs_models.frontend.analyser.SemanticError;
import org.abs_models.frontend.analyser.AnnotationHelper;
import org.abs_models.frontend.analyser.TypeError;
import org.abs_models.frontend.ast.ASTNode;
import org.abs_models.frontend.ast.AssignStmt;
import org.abs_models.frontend.ast.ExpressionStmt;
import org.abs_models.frontend.ast.Model;
import org.abs_models.frontend.ast.NewExp;
import org.abs_models.frontend.ast.PureExp;
import org.abs_models.frontend.ast.Stmt;
import org.abs_models.frontend.ast.VarDeclStmt;

/**
 * @author rudi
 *
 * Checks for type correctness of `new' expression annotations.

 * - DC annotation must be of type ABS.DC.DeploymentComponent
 *
 * - DC annotation cannot be on `new local' expression
 *
 * - Deployment components cannot be created with `new local'
 *
 * - `HTTPName' annotation must be of type String
 */
public class NewExpressionChecker extends DefaultTypeSystemExtension {

    protected NewExpressionChecker(Model m) {
        super(m);
    }

    @Override
    public void checkExpressionStmt(ExpressionStmt expressionStmt) {
        checkDCCorrect(expressionStmt, AnnotationHelper.getAnnotationValueFromName(expressionStmt.getAnnotations(), "ABS.DC.DC"));
        checkHTTPNameCorrect(expressionStmt, AnnotationHelper.getAnnotationValueFromName(expressionStmt.getAnnotations(), "ABS.StdLib.HTTPName"));
    }

    @Override
    public void checkAssignStmt(AssignStmt s) {
        checkDCCorrect(s, AnnotationHelper.getAnnotationValueFromName(s.getAnnotations(), "ABS.DC.DC"));
        checkHTTPNameCorrect(s, AnnotationHelper.getAnnotationValueFromName(s.getAnnotations(), "ABS.StdLib.HTTPName"));
    }

    @Override
    public void checkVarDeclStmt(VarDeclStmt varDeclStmt) {
        checkDCCorrect(varDeclStmt, AnnotationHelper.getAnnotationValueFromName(varDeclStmt.getAnnotations(), "ABS.DC.DC"));
        checkHTTPNameCorrect(varDeclStmt, AnnotationHelper.getAnnotationValueFromName(varDeclStmt.getAnnotations(), "ABS.StdLib.HTTPName"));
    }

    private void checkDCCorrect(ASTNode<?> n, PureExp dc) {
        if (dc == null) return;
        dc.typeCheck(errors);
        if (dc.getType().isUnknownType()
            || !dc.getType().isDeploymentComponentType()) {
            errors.add(new TypeError(n, ErrorMessage.WRONG_DEPLOYMENT_COMPONENT, dc.getType().getQualifiedName()));
        }
    }

    private void checkHTTPNameCorrect(ASTNode<?> n, PureExp restname) {
        if (restname == null) return;
        if (!restname.getType().isStringType()) {
            errors.add(new TypeError(n, ErrorMessage.WRONG_HTTPNAME, restname.getType().getQualifiedName()));
        }
    }

    @Override
    public void checkNewExp(NewExp e) {
        if (e.hasLocal()) {
            if (e.getType().isDeploymentComponentType()) {
                // Don't create a deployment component with "new local"
                errors.add(new SemanticError(e, ErrorMessage.DEPLOYMENT_COMPONENT_NOT_COG, "dummy string to keep constructor happy"));
            }
            Stmt stmt = CompilerUtils.findStmtForExpression(e);
            if (stmt != null) { // should always be true
                if (AnnotationHelper.getAnnotationValueFromName(stmt.getAnnotations(), "ABS.DC.DC") != null) {
                    errors.add(new SemanticError(e, ErrorMessage.DEPLOYMENT_COMPONENT_IGNORED, "dummy string to keep constructor happy"));
                }
            }
        }
    }
}
