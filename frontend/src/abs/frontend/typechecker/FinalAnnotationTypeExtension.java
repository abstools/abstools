package abs.frontend.typechecker;

import abs.frontend.analyser.ErrorMessage;
import abs.frontend.analyser.TypeError;
import abs.frontend.ast.AssignStmt;
import abs.frontend.ast.FieldDecl;
import abs.frontend.ast.Model;
import abs.frontend.ast.VarOrFieldDecl;
import abs.frontend.typechecker.ext.DefaultTypeSystemExtension;

public class FinalAnnotationTypeExtension extends DefaultTypeSystemExtension {
    
    public FinalAnnotationTypeExtension(Model m) {
        super(m);
    }

    @Override
    public void checkAssignStmt(AssignStmt s) {
        if (s.getVar().isFinal()) {
            VarOrFieldDecl d = s.getVar().getDecl();
            String name = d.getName();
            boolean isField = (d instanceof FieldDecl); 
            String kind = isField ? "field" : "variable";
            errors.add(new TypeError(s,ErrorMessage.ASSIGN_TO_FINAL,kind,name));
            
        }
    }

}
