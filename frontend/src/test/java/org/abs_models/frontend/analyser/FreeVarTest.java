/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.frontend.analyser;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

import org.abs_models.frontend.FrontendTest;
import org.abs_models.frontend.ast.ClassDecl;
import org.abs_models.frontend.ast.Exp;
import org.abs_models.frontend.ast.MethodImpl;
import org.abs_models.frontend.ast.ReturnStmt;
import org.abs_models.frontend.ast.Stmt;
import org.junit.Assert;
import org.junit.Test;

public class FreeVarTest extends FrontendTest {

    @Test
    public void dataConstrExp() {
        Exp e = getFirstExp("{ Bool b = True; }");
        assertTrue(e.getFreeVars().isEmpty());
    }

    @Test
    public void varUseExp() {
        Exp e = getSecondExp("{ Bool b = True; Bool c = b; }");
        assertEquals(e.getFreeVars(), "b");
    }

    @Test
    public void letExp() {
        Exp e = getSecondExp("{ Bool b = True; Bool c = let (Bool d) = True in b; }");
        assertEquals(e.getFreeVars(), "b");
    }

    @Test
    public void letExp2() {
        Exp e = getSecondExp("{ Bool b = True; Bool c = let (Bool d) = b in d; }");
        assertEquals(e.getFreeVars(), "b");
    }

    @Test
    public void letExp3() {
        Exp e = getSecondExp("{ Bool b = True; Bool c = let (Bool b) = b in b; }");
        assertEquals(e.getFreeVars(), "b");
    }

    @Test
    public void letExp4() {
        Exp e = getSecondExp("{ Bool b = True; Bool c = let (Bool b) = True in b; }");
        assertTrue(e.getFreeVars().isEmpty());
    }

    @Test
    public void caseExp() {
        Exp e = getSecondExp("{ Bool b = True; Bool c = case True { y => y; }; }");
        assertTrue(e.getFreeVars().isEmpty());
    }

    @Test
    public void unaryExp() {
        Exp e = getSecondExp("{ Bool b = True; Bool c = !b; }");
        assertEquals(e.getFreeVars(), "b");
    }

    @Test
    public void binaryExp() {
        Exp e = getSecondExp("{ Bool b = True; Bool c = b == True; }");
        assertEquals(e.getFreeVars(), "b");
    }

    @Test
    public void binaryExp2() {
        Exp e = getSecondExp("{ Bool b = True; Bool c = True != b; }");
        assertEquals(e.getFreeVars(), "b");
    }

    @Test
    public void binaryExp3() {
        Exp e = getSecondExp("{ Bool b = True; Bool c = b == b; }");
        assertEquals(e.getFreeVars(), "b");
    }

    @Test
    public void callExp() {
        Exp e = getExp("interface I { Unit m(Bool b); } { Bool b = True; I i; Bool c = i.m(b); }", 2);
        assertEquals(e.getFreeVars(), "i", "b");
    }

    @Test
    public void callExp2() {
        Exp e = getExp("interface I { Unit m(Bool b); } { Bool b = True; I i; Bool c = i.m(True); }", 2);
        assertEquals(e.getFreeVars(), "i");
    }

    @Test
    public void asyncCallExp() {
        Exp e = getExp("interface I { Unit m(Bool b); } { Bool b = True; I i; Bool c = i!m(b); }", 2);
        assertEquals(e.getFreeVars(), "i", "b");
    }

    @Test
    public void newExp() {
        Exp e = getSecondExp("class C(Bool b) {}  { Bool b = True; new local C(b); }");
        assertEquals(e.getFreeVars(), "b");
    }

    @Test
    public void getExp() {
        Exp e = getSecondExp("{ Fut<Bool> f; f.get; }");
        assertEquals(e.getFreeVars(), "f");
    }

    @Test
    public void fnApp() {
        Exp e = getSecondExp("def Unit f(Bool b) = Unit; { Bool b; Unit u = f(b); }");
        assertEquals(e.getFreeVars(), "b");
    }

    @Test
    public void constructorApp() {
        Exp e = getSecondExp("data D = C(Bool);{ Bool b; D d = C(b); }");
        assertEquals(e.getFreeVars(), "b");
    }

    @Test
    public void ifExpr() {
        Exp e = getExp("{ Int x = 3; Int y = 4; Int z = 5; Int a = if x == 3 then y else z; }", 3);
        assertEquals(e.getFreeVars(), "x", "y", "z");
    }

    @Test
    public void parFnApp() {
        Exp e = getSecondExp("def Unit f()(Bool b) = Unit; { Bool b = True; Unit u = f()(b); }");
        assertEquals(e.getFreeVars(), "b");
    }

    @Test
    public void parFnAppAnonymousFunction() {
        Exp e = getSecondExp("def Bool f(g)() = g(); def Bool g() = True; { Bool b = True; Bool b2 = f(() => b)(); }");
        assertEquals(e.getFreeVars(), "b");
    }

    @Test
    public void fieldUse() {
        ClassDecl clazz = getFirstClassDecl(assertParse("class C {"
            + "Int i = 0;"
            + "Int m() {"
            + "return i + 1;"
            + "}"
            + "}"));
        MethodImpl method = clazz.lookupMethod("m");
        assertNotNull(method);
        Stmt stmt = method.getBlock().getStmt(0);
        assertTrue(stmt instanceof ReturnStmt);
        ReturnStmt returnStmt = (ReturnStmt) stmt;
        Exp exp = returnStmt.getRetExp();
        assertEquals(exp.getFreeVars(), "i");
    }

    public void assertEquals(Set<String> actual, String... expected) {
        Assert.assertEquals(new HashSet<>(Arrays.asList(expected)), actual);
    }
}
