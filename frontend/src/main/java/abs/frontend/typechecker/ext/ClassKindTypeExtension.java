/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.typechecker.ext;

import java.util.List;

import abs.frontend.analyser.AnnotationHelper;
import abs.frontend.analyser.ErrorMessage;
import abs.frontend.analyser.TypeError;
import abs.frontend.ast.Annotation;
import abs.frontend.ast.ClassDecl;
import abs.frontend.ast.DataConstructorExp;
import abs.frontend.ast.Model;
import abs.frontend.ast.NewExp;
import abs.frontend.typechecker.KindedName;
import abs.frontend.typechecker.KindedName.Kind;

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
