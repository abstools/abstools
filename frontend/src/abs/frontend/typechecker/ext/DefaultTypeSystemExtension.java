package abs.frontend.typechecker.ext;

import abs.frontend.analyser.SemanticErrorList;
import abs.frontend.ast.Model;
import abs.frontend.typechecker.Type;

public abstract class DefaultTypeSystemExtension implements TypeSystemExtension {
    
    protected SemanticErrorList errors;
    protected Model model;
    
    protected DefaultTypeSystemExtension(Model m) {
        model = m;
    }
    
    public void setSemanticErrorList(SemanticErrorList s) {
        errors = s;
    }
    
    public void checkEq(Type lt, Type t) {
        checkAssignable(null, lt, t);
        checkAssignable(null, t, lt);
    }
    
    @Override
    public void finished() {}

}
