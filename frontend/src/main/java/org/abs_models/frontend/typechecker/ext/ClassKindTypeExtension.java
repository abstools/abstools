/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.frontend.typechecker.ext;

import java.util.List;

import org.abs_models.frontend.analyser.AnnotationHelper;
import org.abs_models.frontend.analyser.ErrorMessage;
import org.abs_models.frontend.analyser.TypeError;
import org.abs_models.frontend.ast.Annotation;
import org.abs_models.frontend.ast.ClassDecl;
import org.abs_models.frontend.ast.DataConstructorExp;
import org.abs_models.frontend.ast.Model;
import org.abs_models.frontend.ast.NewExp;
import org.abs_models.frontend.typechecker.KindedName;
import org.abs_models.frontend.typechecker.KindedName.Kind;

public class ClassKindTypeExtension extends DefaultTypeSystemExtension {
    
    public ClassKindTypeExtension(Model m) {
        super(m);
    }

    @Override
    public void checkNewExp(NewExp e) {
        ClassDecl d = (ClassDecl) e.lookup(new KindedName(Kind.CLASS,e.getClassName()));
        List<Annotation> anns = AnnotationHelper.getAnnotationsOfType(d.getAnnotations(), "ABS.StdLib.ClassKindAnnotation");
            
        if (!anns.isEmpty()) {
            String name = ((DataConstructorExp) anns.get(0).getValue()).getDecl().getName();
            if (e.hasLocal()) {
                if (name.equals("COG")) {
                    errors.add(new TypeError(e,ErrorMessage.CLASSKIND_PLAIN,d.getName()));
                }
            } else {
                if (!name.equals("COG")) {
                    errors.add(new TypeError(e,ErrorMessage.CLASSKIND_COG,d.getName()));
                }
            }
        }
    }

}
