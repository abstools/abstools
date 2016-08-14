/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.typechecker.ext;

import abs.frontend.analyser.SemanticCondition;
import abs.frontend.analyser.SemanticConditionList;
import abs.frontend.ast.*;
import abs.frontend.typechecker.Type;
import abs.frontend.typechecker.ext.AdaptDirection;

public class DefaultTypeSystemExtension implements TypeSystemExtension {
    
    protected SemanticConditionList errors;
    protected final Model model;
    
    protected DefaultTypeSystemExtension(Model m) {
        model = m;
    }
    
    public void setSemanticConditionList(SemanticConditionList s) {
        errors = s;
    }

    protected boolean add(SemanticCondition e) {
        return errors.add(e);
    }
    
    public void checkEq(Type lt, Type t, ASTNode<?> origin) {
        checkAssignable(null, null, lt, t, origin);
        checkAssignable(null, null, t, lt, origin);
    }

    @Override
    public void checkModel(Model model) {}
    
    @Override
    public void checkNewExp(NewExp e) {}
    
    @Override
    public void checkMethodCall(Call call) { }
    
    @Override
    public void checkAssignable(Type adaptTo, AdaptDirection dir, Type rht, Type lht, ASTNode<?> n) {    }
    
    @Override
    public void annotateType(Type t, ASTNode<?> originatingNode, ASTNode<?> typeNode) {  }
    
    @Override
    public void finished() {}

    @Override
    public void checkClassDecl(ClassDecl decl) {}

    @Override
    public void checkInterfaceDecl(InterfaceDecl decl) {}

    @Override
    public void checkMethodImpl(MethodImpl method) {}

    @Override
    public void checkReturnStmt(ReturnStmt s) {}
    
    @Override
    public void checkAssignStmt(AssignStmt s) {    }
    
    @Override
    public void checkStmt(Stmt stmt) {}

    @Override
    public void checkAssertStmt(AssertStmt assertStmt) {}

    @Override
    public void checkWhileStmt(WhileStmt whileStmt) {}

    @Override
    public void checkVarDeclStmt(VarDeclStmt varDeclStmt) {}

    @Override
    public void checkDurationStmt(DurationStmt durationStmt) {}

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

    @Override
    public void checkOverride(MethodSig impl, MethodSig overriden) {    }
}
