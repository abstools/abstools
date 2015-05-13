/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.typechecker.ext;

import abs.frontend.analyser.ErrorMessage;
import abs.frontend.analyser.TypeError;
import abs.frontend.ast.AssignStmt;
import abs.frontend.ast.FieldDecl;
import abs.frontend.ast.Model;
import abs.frontend.ast.TypedVarOrFieldDecl;
import abs.frontend.ast.VarOrFieldDecl;

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
