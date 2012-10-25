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

public class FinalAnnotationTypeExtension extends DefaultTypeSystemExtension {
    
    public FinalAnnotationTypeExtension(Model m) {
        super(m);
    }

    @Override
    public void checkAssignStmt(AssignStmt s) {
        // There'll never be PatternVars in an assignment.
        TypedVarOrFieldDecl d = (TypedVarOrFieldDecl) s.getVar().getDecl();
        // Not sure if this code will encounter delta bodies:
        if (d != null && d.isFinal()) {
            String name = d.getName();
            boolean isField = (d instanceof FieldDecl); 
            String kind = isField ? "field" : "variable";
            add(new TypeError(s,ErrorMessage.ASSIGN_TO_FINAL,kind,name));
        }
    }

}
