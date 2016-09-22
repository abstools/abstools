/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.typechecker.ext;

import abs.common.CompilerUtils;
import abs.frontend.analyser.ErrorMessage;
import abs.frontend.analyser.SemanticError;
import abs.frontend.analyser.TypeError;
import abs.frontend.ast.ASTNode;
import abs.frontend.ast.AssignStmt;
import abs.frontend.ast.ExpressionStmt;
import abs.frontend.ast.Model;
import abs.frontend.ast.NewExp;
import abs.frontend.ast.PureExp;
import abs.frontend.ast.Stmt;
import abs.frontend.ast.VarDeclStmt;

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
        checkDCCorrect(expressionStmt, CompilerUtils.getAnnotationValueFromName(expressionStmt.getAnnotations(), "ABS.DC.DC"));
        checkHTTPNameCorrect(expressionStmt, CompilerUtils.getAnnotationValueFromName(expressionStmt.getAnnotations(), "ABS.StdLib.RESTName"));
    }

    @Override
    public void checkAssignStmt(AssignStmt s) {
        checkDCCorrect(s, CompilerUtils.getAnnotationValueFromName(s.getAnnotations(), "ABS.DC.DC"));
        checkHTTPNameCorrect(s, CompilerUtils.getAnnotationValueFromName(s.getAnnotations(), "ABS.StdLib.RESTName"));
    }

    @Override
    public void checkVarDeclStmt(VarDeclStmt varDeclStmt) {
        checkDCCorrect(varDeclStmt, CompilerUtils.getAnnotationValueFromName(varDeclStmt.getAnnotations(), "ABS.DC.DC"));
        checkHTTPNameCorrect(varDeclStmt, CompilerUtils.getAnnotationValueFromName(varDeclStmt.getAnnotations(), "ABS.StdLib.RESTName"));
    }

    private void checkDCCorrect(ASTNode<?> n, PureExp dc) {
        if (dc == null) return;
        if (!dc.getType().isDeploymentComponentType()) {
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
                if (CompilerUtils.getAnnotationValueFromName(stmt.getAnnotations(), "ABS.DC.DC") != null) {
                    errors.add(new SemanticError(e, ErrorMessage.DEPLOYMENT_COMPONENT_IGNORED, "dummy string to keep constructor happy"));
                }
            }
        }
    }
}
