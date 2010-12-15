package abs.frontend.typechecker.ext;

import abs.frontend.analyser.SemanticErrorList;
import abs.frontend.ast.ASTNode;
import abs.frontend.ast.Call;
import abs.frontend.ast.ClassDecl;
import abs.frontend.ast.Model;
import abs.frontend.ast.NewExp;
import abs.frontend.typechecker.KindedName;
import abs.frontend.typechecker.KindedName.Kind;
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
        checkAssignable(null, lt, t, null);
        checkAssignable(null, t, lt, null);
    }
    
    @Override
    public void checkNewExp(NewExp e) {}
    
    @Override
    public void checkMethodCall(Call call) { }
    
    @Override
    public void checkAssignable(Type adaptTo, Type rht, Type lht, ASTNode<?> n) {    }
    
    @Override
    public void annotateType(Type t, ASTNode<?> originatingNode, ASTNode<?> typeNode) {  }
    
    @Override
    public void finished() {}
    
}
