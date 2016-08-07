/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.typechecker.ext;

import abs.frontend.analyser.SemanticConditionList;
import abs.frontend.ast.*;
import abs.frontend.typechecker.Type;
import abs.frontend.typechecker.ext.AdaptDirection;

public interface TypeSystemExtension {

    void checkModel(Model model);

    void checkClassDecl(ClassDecl decl);

    void checkInterfaceDecl(InterfaceDecl decl);

    void checkMethodImpl(MethodImpl method);

    void checkAssignable(Type adaptTo, AdaptDirection dir, Type rht, Type lht, ASTNode<?> n);

    void annotateType(Type t, ASTNode<?> orinatingNode, ASTNode<?> typeNode);

    void checkMethodCall(Call call);
    
    void checkNewExp(NewExp e);
    
    void checkEq(Type lt, Type t, ASTNode<?> origin);

    void setSemanticConditionList(SemanticConditionList errors);

    void finished();

    void checkStmt(Stmt s);

    void checkAssignStmt(AssignStmt s);

    void checkReturnStmt(ReturnStmt s);

    void checkAssertStmt(AssertStmt assertStmt);

    void checkWhileStmt(WhileStmt whileStmt);

    void checkVarDeclStmt(VarDeclStmt varDeclStmt);

    void checkSuspendStmt(SuspendStmt suspendStmt);

    void checkIfStmt(IfStmt ifStmt);

    void checkDurationStmt(DurationStmt durationStmt);

    void checkExpressionStmt(ExpressionStmt expressionStmt);

    void checkBlock(Block block);

    void checkAwaitStmt(AwaitStmt awaitStmt);

    void checkGetExp(GetExp e);

    void checkOverride(MethodSig impl, MethodSig overriden);

}
