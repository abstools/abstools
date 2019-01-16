/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.frontend.typechecker.ext;

import org.abs_models.frontend.analyser.AnnotationHelper;
import org.abs_models.frontend.analyser.ErrorMessage;
import org.abs_models.frontend.analyser.TypeError;
import org.abs_models.frontend.ast.ASTNode;
import org.abs_models.frontend.ast.AssignStmt;
import org.abs_models.frontend.ast.ExpressionStmt;
import org.abs_models.frontend.ast.Model;
import org.abs_models.frontend.ast.PureExp;
import org.abs_models.frontend.ast.ReturnStmt;
import org.abs_models.frontend.ast.VarDeclStmt;

public class SizeAnnotationChecker extends DefaultTypeSystemExtension {

    protected SizeAnnotationChecker(Model m) {
        super(m);
    }

    @Override
    public void checkExpressionStmt(ExpressionStmt e) {
        checkSizeAnnotationCorrect(e, AnnotationHelper.getAnnotationValueFromName(e.getAnnotations(), "ABS.DC.DataSize"));
    }

    @Override
    public void checkAssignStmt(AssignStmt s) {
        checkSizeAnnotationCorrect(s, AnnotationHelper.getAnnotationValueFromName(s.getAnnotations(), "ABS.DC.DataSize"));
    }

    @Override
    public void checkVarDeclStmt(VarDeclStmt v) {
        checkSizeAnnotationCorrect(v, AnnotationHelper.getAnnotationValueFromName(v.getAnnotations(), "ABS.DC.DataSize"));
    }

    @Override
    public void checkReturnStmt(ReturnStmt s) {
        checkSizeAnnotationCorrect(s, AnnotationHelper.getAnnotationValueFromName(s.getAnnotations(), "ABS.DC.DataSize"));
    }

    private void checkSizeAnnotationCorrect(ASTNode<?> n, PureExp size) {
        if (size == null) return;
        size.typeCheck(errors);
        if (!size.getType().isNumericType()) {
            errors.add(new TypeError(n, ErrorMessage.WRONG_SIZE_ANNOTATION_TYPE,
                                     size.getType().getQualifiedName()));
        }
    }
}
