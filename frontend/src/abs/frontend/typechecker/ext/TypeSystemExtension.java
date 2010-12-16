package abs.frontend.typechecker.ext;

import abs.frontend.analyser.SemanticErrorList;
import abs.frontend.ast.ASTNode;
import abs.frontend.ast.AssignStmt;
import abs.frontend.ast.Call;
import abs.frontend.ast.NewExp;
import abs.frontend.typechecker.Type;

public interface TypeSystemExtension {

    void checkAssignable(Type adaptTo, Type rht, Type lht, ASTNode<?> n);

    void annotateType(Type t, ASTNode<?> orinatingNode, ASTNode<?> typeNode);

    void checkMethodCall(Call call);
    
    void checkNewExp(NewExp e);
    
    void checkEq(Type lt, Type t);

    void setSemanticErrorList(SemanticErrorList errors);

    void finished();

    void checkAssignStmt(AssignStmt s);

}
