/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.frontend;

import static org.abs_models.ABSTest.Config.EXPECT_TYPE_ERROR;
import static org.abs_models.ABSTest.Config.EXPECT_WARNING;
import static org.abs_models.ABSTest.Config.TYPE_CHECK;
import static org.junit.Assert.assertEquals;

import java.io.IOException;

import org.abs_models.ABSTest;
import org.abs_models.backend.common.InternalBackendException;
import org.abs_models.common.WrongProgramArgumentException;
import org.abs_models.frontend.analyser.ErrorMessage;
import org.abs_models.frontend.analyser.SemanticCondition;
import org.abs_models.frontend.analyser.SemanticConditionList;
import org.abs_models.frontend.ast.AssignStmt;
import org.abs_models.frontend.ast.CaseBranch;
import org.abs_models.frontend.ast.CaseExp;
import org.abs_models.frontend.ast.ClassDecl;
import org.abs_models.frontend.ast.Decl;
import org.abs_models.frontend.ast.Exp;
import org.abs_models.frontend.ast.ExpFunctionDef;
import org.abs_models.frontend.ast.ExpressionStmt;
import org.abs_models.frontend.ast.FunctionDecl;
import org.abs_models.frontend.ast.Model;
import org.abs_models.frontend.ast.ParametricFunctionDecl;
import org.abs_models.frontend.ast.Pattern;
import org.abs_models.frontend.ast.Stmt;
import org.abs_models.frontend.ast.VarDeclStmt;
import org.abs_models.frontend.typechecker.Type;

public class FrontendTest extends ABSTest {

    protected Model assertTypeCheckFileOk(String fileName) throws IOException, WrongProgramArgumentException, InternalBackendException {
	return assertParseFileOk(fileName, TYPE_CHECK);
    }

    protected Exp getFirstExp(String absCode) {
        Model m = assertParse(absCode);
        return getFirstExp(m);
    }

    protected Exp getSecondExp(String absCode) {
        return getSecondExp(assertParse(absCode));
    }

    protected Exp getExp(String absCode, int i) {
        return getExp(assertParse(absCode), i);
    }

    protected Exp getSecondExp(Model m) {
        return getExp(m, 1);
    }

    protected Exp getFirstExp(Model m) {
        return getExp(m, 0);
    }

    protected Exp getExp(Model m, int i) {
        Stmt s = m.getMainBlock().getStmt(i);
        if (s instanceof AssignStmt)
            return ((AssignStmt) s).getValue();
        if (s instanceof ExpressionStmt)
            return ((ExpressionStmt) s).getExp();
        if (s instanceof VarDeclStmt)
            return ((VarDeclStmt) s).getVarDecl().getInitExp();
        throw new IllegalArgumentException();
    }

    protected ClassDecl getFirstClassDecl(Model m) {
        for (Decl d : m.getCompilationUnit(m.getNumCompilationUnit()-1).getModuleDecl(0).getDecls()) {
            if (d instanceof ClassDecl) 
                return (ClassDecl) d;
        }
        throw new IllegalArgumentException("No ClassDecl found");
    }
    
    protected Exp getFirstCaseExpr(Model m) {
        CaseExp ce = (CaseExp) getFirstFunctionExpr(m);
        CaseBranch b = ce.getBranch(0);
        return b.getRight();
    }

    protected Exp getSecondCaseExpr(Model m) {
        CaseExp ce = (CaseExp) getFirstFunctionExpr(m);
        CaseBranch b = ce.getBranch(1);
        return b.getRight();
    }

    protected Pattern getFirstCasePattern(Model m) {
        CaseExp ce = (CaseExp) getFirstFunctionExpr(m);
        CaseBranch b = ce.getBranch(0);
        return b.getLeft();
    }
    
    protected Decl getLastDecl(Model m, Class<?> clazz) {
        Decl lastMatching = null;
        for (Decl d : m.getDecls()) {
            if (clazz.isInstance(d)) {
                lastMatching = d;
            }
        }
        if (lastMatching == null)
            throw new IllegalArgumentException("The model does not contain any " + clazz.getSimpleName());
        else
            return lastMatching;
    }

    protected FunctionDecl getLastFunctionDecl(Model m) {
        return (FunctionDecl) getLastDecl(m, FunctionDecl.class);
    }

    protected ParametricFunctionDecl getLastParametricFunctionDecl(Model m) {
        return (ParametricFunctionDecl) getLastDecl(m, ParametricFunctionDecl.class);
    }

    protected Exp getFirstFunctionExpr(Model m) {
        return ((ExpFunctionDef) getLastFunctionDecl(m).getFunctionDef()).getRhs();
    }

    protected Type getTypeOfFirstAssignment(Model m) {
        return getTypeOfNthAssignment(m, 1);
    }

    protected Type getTypeOfNthAssignment(Model m, int n) {
        int count = 0;
        for (Stmt s : m.getMainBlock().getStmts()) {
            Type t = null;
            if (s instanceof AssignStmt) {
                AssignStmt as = (AssignStmt) s;
                t = as.getValue().getType();
            } else if (s instanceof VarDeclStmt) {
                VarDeclStmt vd = (VarDeclStmt) s;
                if (vd.getVarDecl().hasInitExp()) {
                    t = vd.getVarDecl().getInitExp().getType();
                }
            }
            if (t != null) {
                count++;
                if (count == n) {
                    return t;
                }
            }
        }
        return null;
    }

    protected Type getTypeOfFirstVariableDeclaration(Model m) {
        return getTypeOfNthVariableDeclaration(m, 1);
    }

    protected Type getTypeOfNthVariableDeclaration(Model m, int n) {
        int count = 0;
        for (Stmt s : m.getMainBlock().getStmts()) {
            Type t = null;
            if (s instanceof VarDeclStmt) {
                VarDeclStmt vd = (VarDeclStmt) s;
                t = vd.getVarDecl().getType();
            }
            if (t != null) {
                count++;
                if (count == n) {
                    return t;
                }
            }
        }
        return null;
    }
    
    protected void assertTypeOK(String absCode) {
        assertTypeErrors(absCode, new Config[0]);
    }

    protected SemanticCondition assertTypeErrors(String absCode) {
        return assertTypeErrors(absCode, EXPECT_TYPE_ERROR);
    }

    protected void assertTypeErrors(String absCode, ErrorMessage expected) {
        SemanticCondition e = assertTypeErrors(absCode, EXPECT_TYPE_ERROR);
        assertEquals(expected,e.msg);
    }

    /**
     * Check for and return the first error occurring in 'absCode', or
     * null if none found.  Produces a test failure if 'config'
     * contains EXPECT_TYPE_ERROR but no error found.  Produces a test
     * failure if 'config' contains EXPECT_WARNING but no warning
     * found.
     * @param absCode - the test case source code
     * @param config - flags
     * @return The first error or null
     */
    protected SemanticCondition assertTypeErrors(String absCode, Config... config) {
        Model m = assertParse(absCode, config);
        String msg = "";
        SemanticConditionList l = m.typeCheck();
        if (l.containsErrors()) {
            msg = l.getFirstError().getMsgWithHint(absCode);
        } else if (l.containsWarnings() && isSet(EXPECT_WARNING, config)) {
            msg = l.getFirstWarning().getMsgWithHint(absCode);
        }

        assertEquals(msg, isSet(EXPECT_TYPE_ERROR, config), l.containsErrors());
        if (isSet(EXPECT_WARNING, config)) {
            assertEquals(msg, isSet(EXPECT_WARNING, config), l.containsWarnings());
        }
        return l.containsErrors() ? l.getFirstError() : null;
    }

    protected SemanticCondition assertWarnings(String absCode) {
        return assertTypeErrors(absCode, EXPECT_WARNING);
    }
}
