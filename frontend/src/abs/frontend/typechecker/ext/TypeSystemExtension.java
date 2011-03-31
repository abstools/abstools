/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.typechecker.ext;

import abs.frontend.analyser.SemanticErrorList;
import abs.frontend.ast.ASTNode;
import abs.frontend.ast.AssertStmt;
import abs.frontend.ast.AssignStmt;
import abs.frontend.ast.AwaitStmt;
import abs.frontend.ast.Block;
import abs.frontend.ast.Call;
import abs.frontend.ast.ExpressionStmt;
import abs.frontend.ast.GetExp;
import abs.frontend.ast.IfStmt;
import abs.frontend.ast.MethodImpl;
import abs.frontend.ast.MethodSig;
import abs.frontend.ast.NewExp;
import abs.frontend.ast.ReturnStmt;
import abs.frontend.ast.Stmt;
import abs.frontend.ast.SuspendStmt;
import abs.frontend.ast.VarDeclStmt;
import abs.frontend.ast.WhileStmt;
import abs.frontend.typechecker.Type;
import abs.frontend.typechecker.ext.AdaptDirection;

public interface TypeSystemExtension {

    void checkAssignable(Type adaptTo, AdaptDirection dir, Type rht, Type lht, ASTNode<?> n);

    void annotateType(Type t, ASTNode<?> orinatingNode, ASTNode<?> typeNode);

    void checkMethodCall(Call call);
    
    void checkNewExp(NewExp e);
    
    void checkEq(Type lt, Type t, ASTNode<?> origin);

    void setSemanticErrorList(SemanticErrorList errors);

    void finished();

    void checkAssignStmt(AssignStmt s);

    void checkReturnStmt(ReturnStmt s);

    void checkAssertStmt(AssertStmt assertStmt);

    void checkWhileStmt(WhileStmt whileStmt);

    void checkVarDeclStmt(VarDeclStmt varDeclStmt);

    void checkSuspendStmt(SuspendStmt suspendStmt);

    void checkIfStmt(IfStmt ifStmt);

    void checkExpressionStmt(ExpressionStmt expressionStmt);

    void checkBlock(Block block);

    void checkAwaitStmt(AwaitStmt awaitStmt);

    void checkGetExp(GetExp e);

    void checkOverride(MethodSig impl, MethodSig overriden);

}
