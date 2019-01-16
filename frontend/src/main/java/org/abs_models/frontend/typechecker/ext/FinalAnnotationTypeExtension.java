/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.frontend.typechecker.ext;

import org.abs_models.frontend.analyser.ErrorMessage;
import org.abs_models.frontend.analyser.TypeError;
import org.abs_models.frontend.ast.AssignStmt;
import org.abs_models.frontend.ast.FieldDecl;
import org.abs_models.frontend.ast.Model;
import org.abs_models.frontend.ast.TypedVarOrFieldDecl;
import org.abs_models.frontend.ast.VarOrFieldDecl;

public class FinalAnnotationTypeExtension extends DefaultTypeSystemExtension {
    
    public FinalAnnotationTypeExtension(Model m) {
        super(m);
    }

    @Override
    public void checkAssignStmt(AssignStmt s) {
        VarOrFieldDecl decl = s.getVar().getDecl();
        if (decl instanceof TypedVarOrFieldDecl) {
            TypedVarOrFieldDecl d = (TypedVarOrFieldDecl)decl;
            // Not sure if this code will encounter delta bodies:
            if (d.isFinal()) {
                String name = d.getName();
                boolean isField = (d instanceof FieldDecl);
                String kind = isField ? "field" : "variable";
                add(new TypeError(s,ErrorMessage.ASSIGN_TO_FINAL,kind,name));
            }
        } else {
            // It's a PatternVarDecl.  Assume these are never final.
        }
    }

}
