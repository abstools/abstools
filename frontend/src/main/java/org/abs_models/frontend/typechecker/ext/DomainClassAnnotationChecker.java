/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.frontend.typechecker.ext;

import java.util.HashSet;
import java.util.Set;

import org.abs_models.frontend.analyser.AnnotationHelper;
import org.abs_models.frontend.analyser.ErrorMessage;
import org.abs_models.frontend.analyser.TypeError;
import org.abs_models.frontend.ast.ClassDecl;
import org.abs_models.frontend.ast.FieldDecl;
import org.abs_models.frontend.ast.Model;
import org.abs_models.frontend.ast.ParamDecl;
import org.abs_models.frontend.ast.PureExp;

/// Type-check `DomainClass` annotations: must be of type String (but
/// doesn't need to be a literal string).
public class DomainClassAnnotationChecker extends DefaultTypeSystemExtension {

    protected DomainClassAnnotationChecker(Model m) {
        super(m);
    }

    @Override
    public void checkClassDecl(ClassDecl decl) {
        PureExp domainClassAnnotation = AnnotationHelper.getAnnotationValueFromName(decl.getAnnotations(), "ABS.StdLib.DomainClass");
        if (domainClassAnnotation == null) return;
        domainClassAnnotation.typeCheck(errors); // will complain about unknown variables
        if (!domainClassAnnotation.getType().isStringType()) {
            errors.add(new TypeError(domainClassAnnotation,
                ErrorMessage.WRONG_DOMAIN_CLASS_ANNOTATION_TYPE,
                domainClassAnnotation.getType().getQualifiedName()));
        }
    }
}
