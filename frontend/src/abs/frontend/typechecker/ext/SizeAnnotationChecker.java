/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
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
import abs.frontend.ast.ReturnStmt;
import abs.frontend.ast.VarDeclStmt;

public class SizeAnnotationChecker extends DefaultTypeSystemExtension {

    protected SizeAnnotationChecker(Model m) {
        super(m);
    }

    @Override
    public void checkExpressionStmt(ExpressionStmt e) {
        checkSizeAnnotationCorrect(e, CompilerUtils.getAnnotationValueFromName(e.getAnnotations(), "ABS.DC.Size"));
    }

    @Override
    public void checkAssignStmt(AssignStmt s) {
        checkSizeAnnotationCorrect(s, CompilerUtils.getAnnotationValueFromName(s.getAnnotations(), "ABS.DC.Size"));
    }

    @Override
    public void checkVarDeclStmt(VarDeclStmt v) {
        checkSizeAnnotationCorrect(v, CompilerUtils.getAnnotationValueFromName(v.getAnnotations(), "ABS.DC.Size"));
    }

    @Override
    public void checkReturnStmt(ReturnStmt s) {
        checkSizeAnnotationCorrect(s, CompilerUtils.getAnnotationValueFromName(s.getAnnotations(), "ABS.DC.Size"));
    }

    private void checkSizeAnnotationCorrect(ASTNode<?> n, PureExp size) {
        if (size == null) return;
        if (!size.getType().isNumericType()) {
            errors.add(new TypeError(n, ErrorMessage.WRONG_SIZE_ANNOTATION_TYPE,
                                     size.getType().getQualifiedName()));
        }
    }
}
