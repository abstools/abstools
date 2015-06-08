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
 * Checks for type of DC annotation (must be DeploymentComponent) and for creation of deployment components within another cog (not possible).
 */
public class DeploymentComponentChecker extends DefaultTypeSystemExtension {

    protected DeploymentComponentChecker(Model m) {
        super(m);
    }

    @Override
    public void checkExpressionStmt(ExpressionStmt expressionStmt) {
        checkDCCorrect(expressionStmt, CompilerUtils.getAnnotationValue(expressionStmt.getAnnotations(), "DC"));
    }

    @Override
    public void checkAssignStmt(AssignStmt s) {
        checkDCCorrect(s, CompilerUtils.getAnnotationValue(s.getAnnotations(), "DC"));
    }

    @Override
    public void checkVarDeclStmt(VarDeclStmt varDeclStmt) {
        checkDCCorrect(varDeclStmt, CompilerUtils.getAnnotationValue(varDeclStmt.getAnnotations(), "DC"));
    }

    private void checkDCCorrect(ASTNode<?> n, PureExp dc) {
        if (dc == null) return;
        if (!dc.getType().isDeploymentComponentType()) {
            errors.add(new TypeError(n, ErrorMessage.WRONG_DEPLOYMENT_COMPONENT, dc.getType().getQualifiedName()));
        }
    }
    
    @Override
    public void checkNewExp(NewExp e) {
        if (e.hasLocal()) {
            if (e.getType().isDeploymentComponentType()) {
                // Don't create a deployment component with "new local"
                errors.add(new SemanticError(e, ErrorMessage.DEPLOYMENT_COMPONENT_NOT_COG, "dummy string to keep constructor happy"));
            }
            ASTNode parent = e.getParent();
            if (parent instanceof Stmt) { // should always be true
                Stmt stmt = (Stmt) parent;
                if (CompilerUtils.getAnnotationValue(stmt.getAnnotations(), "DC") != null) {
                    errors.add(new SemanticError(e, ErrorMessage.DEPLOYMENT_COMPONENT_IGNORED, "dummy string to keep constructor happy"));
                }
            }
        }
    }
      
}
