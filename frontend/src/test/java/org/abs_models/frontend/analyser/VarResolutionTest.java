/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.frontend.analyser;

import static org.hamcrest.CoreMatchers.instanceOf;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;

import org.abs_models.frontend.FrontendTest;
import org.abs_models.frontend.ast.ClassDecl;
import org.abs_models.frontend.ast.ConstructorPattern;
import org.abs_models.frontend.ast.Exp;
import org.abs_models.frontend.ast.FieldDecl;
import org.abs_models.frontend.ast.FieldUse;
import org.abs_models.frontend.ast.LetExp;
import org.abs_models.frontend.ast.Model;
import org.abs_models.frontend.ast.ModuleDecl;
import org.abs_models.frontend.ast.NegExp;
import org.abs_models.frontend.ast.ParamDecl;
import org.abs_models.frontend.ast.PatternVar;
import org.abs_models.frontend.ast.PatternVarDecl;
import org.abs_models.frontend.ast.ReturnStmt;
import org.abs_models.frontend.ast.VarDecl;
import org.abs_models.frontend.ast.VarOrFieldDecl;
import org.abs_models.frontend.ast.VarUse;
import org.abs_models.frontend.typechecker.KindedName;
import org.junit.Test;

public class VarResolutionTest extends FrontendTest {
    @Test
    public void testLocalVar() {
        Exp e = getSecondExp("interface I { } { I i = null; i = i; }");
        VarUse u = (VarUse) e;
        VarDecl decl = (VarDecl) u.getDecl();
        assertEquals("i", decl.getName());
    }

    @Test
    public void testPatternVar() {
        Model m = assertParse(" def Bool f(Bool b) = case b { True => False; x => !x; };");
        NegExp ne = (NegExp) getSecondCaseExpr(m);
        VarUse v = (VarUse) ne.getOperand();
        PatternVarDecl decl = (PatternVarDecl) v.getDecl();
        assertEquals("x", decl.getName());
    }

    @Test
    public void testNestedPatternVar() {
        Model m = assertParse("data Foo = Bar(Bool); def Bool m(Foo f) = case f { Bar(y) => y; };");
        assertThat(getFirstCaseExpr(m),instanceOf(VarUse.class));
        ConstructorPattern p = (ConstructorPattern) getFirstCasePattern(m);
        PatternVarDecl decl = ((PatternVar) p.getParam(0)).getVar();
        assertEquals("y", decl.getName());
    }

    @Test
    public void testFunctionParam() {
        Model m = assertParse(" def Bool f(Bool b) = b;");
        VarUse u = (VarUse) getFirstFunctionExpr(m);
        ParamDecl d = (ParamDecl) u.getDecl();
        assertEquals("b", d.getName());
    }

    @Test
    public void testLetExp() {
        Model m = assertParse(" def Bool f(Bool b) = let (Bool x) = b in x;");
        LetExp e = (LetExp) getFirstFunctionExpr(m);
        VarOrFieldDecl decl = e.getVar();
        VarUse u = (VarUse) e.getExp();
        assertEquals(decl, u.getDecl());
    }

    @Test
    public void testNestedLetExp() {
        Model m = assertParse(" def Bool f(Bool b) = let (Bool x) = let (Bool y) = b in y in x;");
        LetExp e = (LetExp) getFirstFunctionExpr(m);
        VarOrFieldDecl decl = e.getVar();
        VarUse u = (VarUse) e.getExp();
        assertEquals(decl, u.getDecl());
    }

    @Test
    public void testNestedLetExp2() {
        Model m = assertParse(" def Bool f(Bool b) = let (Bool x) = let (Bool x) = b in x in x;");
        LetExp e = (LetExp) getFirstFunctionExpr(m);
        VarOrFieldDecl decl = e.getVar();
        VarUse u = (VarUse) e.getExp();
        assertEquals(decl, u.getDecl());
    }

    @Test
    public void testNestedLetExp3() {
        Model m = assertParse(" def Bool f(Bool b) = let (Bool x) = b in let (Bool y) = b in x;");
        LetExp e = (LetExp) getFirstFunctionExpr(m);
        LetExp e2 = (LetExp) e.getExp();
        VarOrFieldDecl decl = e.getVar();
        VarUse u = (VarUse) e2.getExp();
        assertEquals(decl, u.getDecl());
    }

    @Test
    public void testNestedLetExp4() {
        Model m = assertParse(" def Bool f(Bool b) = let (Bool x) = b in let (Bool x) = b in x;");
        LetExp e = (LetExp) getFirstFunctionExpr(m);
        LetExp e2 = (LetExp) e.getExp();
        VarOrFieldDecl decl = e2.getVar();
        VarUse u = (VarUse) e2.getExp();
        assertEquals(decl, u.getDecl());
    }

    @Test
    public void testNestedLetExp5() {
        Model m = assertParse("def Bool f(Bool b) = let (Bool x) = b in let (Bool x) = x in x;");
        LetExp e = (LetExp) getFirstFunctionExpr(m);
        LetExp e2 = (LetExp) e.getExp();
        VarOrFieldDecl decl = e.getVar();
        VarUse u = (VarUse) e2.getVal();
        assertEquals(decl, u.getDecl());
    }

    @Test
    public void testFieldUse() {
        Model m = assertParse(" class C { Bool f; Bool m() { return this.f; } }");
        ClassDecl d = (ClassDecl) getTestModule(m).lookup(new KindedName(KindedName.Kind.CLASS, "UnitTest.C"));
        FieldDecl f = d.getField(0);
        ReturnStmt s = (ReturnStmt) d.getMethod(0).getBlock().getStmt(0);
        assertEquals(f, ((FieldUse) s.getRetExp()).getDecl());
    }

    static private ModuleDecl getTestModule(Model m) {
        ModuleDecl md = m.lookupModule("UnitTest");
        assertNotNull("Module UnitTest not found", md);
        return md;
    }
}
