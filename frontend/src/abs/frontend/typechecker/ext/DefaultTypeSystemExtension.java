package abs.frontend.typechecker.ext;

import abs.frontend.analyser.SemanticErrorList;
import abs.frontend.ast.ASTNode;
import abs.frontend.ast.AssertStmt;
import abs.frontend.ast.AssignStmt;
import abs.frontend.ast.AwaitStmt;
import abs.frontend.ast.Block;
import abs.frontend.ast.Call;
import abs.frontend.ast.ClassDecl;
import abs.frontend.ast.ExpressionStmt;
import abs.frontend.ast.GetExp;
import abs.frontend.ast.IfStmt;
import abs.frontend.ast.Model;
import abs.frontend.ast.NewExp;
import abs.frontend.ast.ReturnStmt;
import abs.frontend.ast.Stmt;
import abs.frontend.ast.SuspendStmt;
import abs.frontend.ast.VarDeclStmt;
import abs.frontend.ast.WhileStmt;
import abs.frontend.typechecker.KindedName;
import abs.frontend.typechecker.KindedName.Kind;
import abs.frontend.typechecker.Type;

public class DefaultTypeSystemExtension implements TypeSystemExtension {
    
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

    @Override
    public void checkReturnStmt(ReturnStmt s) {}
    
    @Override
    public void checkAssignStmt(AssignStmt s) {    }
    
    @Override
    public void checkAssertStmt(AssertStmt assertStmt) {}

    @Override
    public void checkWhileStmt(WhileStmt whileStmt) {}

    @Override
    public void checkVarDeclStmt(VarDeclStmt varDeclStmt) {}

    @Override
    public void checkSuspendStmt(SuspendStmt suspendStmt) {}

    @Override
    public void checkIfStmt(IfStmt ifStmt) {}

    @Override
    public void checkExpressionStmt(ExpressionStmt expressionStmt) {}

    @Override
    public void checkBlock(Block block) {}

    @Override
    public void checkAwaitStmt(AwaitStmt awaitStmt) {}
 
    @Override
    public void checkGetExp(GetExp e) {
    }
}
