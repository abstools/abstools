package org.abs_models.frontend.typesystem;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

import org.abs_models.ABSTest;
import org.abs_models.frontend.FrontendTest;
import org.abs_models.frontend.ast.*;
import org.abs_models.frontend.typechecker.KindedName;
import org.abs_models.frontend.typechecker.nullable.SimpleSet;
import org.junit.Test;

public class NullableTests extends FrontendTest {
    @Test
    public void test1() {
        Model m = assertParse("interface I { Unit m(I i); } class C implements I { Unit m(I i) { i!m(this); skip; } }");
        CompilationUnit cu = m.getCompilationUnit(0);
        ClassDecl d = (ClassDecl) getTestModule(m).lookup(new KindedName(KindedName.Kind.CLASS, "UnitTest.C"));
        MethodImpl met = d.getMethod(0);
        Block b = met.getBlock();
        System.out.println(b.getStmt(0).nonNull_in());
        System.out.println(b.getStmt(0).nonNull_out());
        System.out.println(b.getStmt(1).nonNull_out());
        assertTrue(true);
    }

    @Test
    public void testMethodDeclNewExp() {
        MethodImpl met = getMethod("interface I { Unit m(); } class C implements I { Unit m() { I i = new C(); } }");

        Block b = met.getBlock();
        VarDeclStmt s = (VarDeclStmt) b.getStmt(0);
        VarDecl d = s.getVarDecl();

        assertTrue(b.getStmt(0).nonNull_out().contains(d));
    }

    @Test
    public void testMethodAssignNewExp() {
        MethodImpl met = getMethod("interface I { Unit m(I i); } class C implements I { Unit m(I i) { i = new C(); } }");

        ParamDecl p = met.getMethodSig().getParam(0);
        Block b = met.getBlock();

        SimpleSet<VarOrFieldDecl> nonNull = b.getStmt(0).nonNull_out();
        assertTrue(nonNull.contains(p));
        assertEquals(1, nonNull.size());
    }

    @Test
    public void testMethodAssignThis() {
        MethodImpl met = getMethod("interface I { Unit m(I i); } class C implements I { Unit m(I i) { i = this; } }");

        ParamDecl p = met.getMethodSig().getParam(0);
        Block b = met.getBlock();

        SimpleSet<VarOrFieldDecl> nonNull = b.getStmt(0).nonNull_out();
        assertTrue(nonNull.contains(p));
        assertEquals(1, nonNull.size());
    }

    @Test
    public void testMethodSecondCall() {
        MethodImpl met = getMethod("interface I { Unit m(I i); } class C implements I { Unit m(I i) { i.m(this); } }");

        ParamDecl p = met.getMethodSig().getParam(0);
        Block b = met.getBlock();

        SimpleSet<VarOrFieldDecl> nonNull1 = b.getStmt(0).nonNull_in();
        SimpleSet<VarOrFieldDecl> nonNull2 = b.getStmt(0).nonNull_out();
        assertTrue(nonNull1.isEmpty());
        assertTrue(nonNull2.contains(p));
    }

    @Test
    public void testMethodAssert() {
        MethodImpl met = getMethod("interface I { Unit m(I i); } class C implements I { Unit m(I i) { assert i != null; skip; } }");

        ParamDecl p = met.getMethodSig().getParam(0);
        Block b = met.getBlock();
        Stmt skip = b.getStmt(1);

        SimpleSet<VarOrFieldDecl> nonNull1 = b.getStmt(0).nonNull_in();
        SimpleSet<VarOrFieldDecl> nonNull2 = b.getStmt(0).nonNull_out();
        assertTrue(nonNull1.isEmpty());
        assertTrue(nonNull2.isEmpty());
        assertTrue(skip.nonNull_in().contains(p));
    }

    @Test
    public void testMethodCondition() {
        MethodImpl met = getMethod("interface I { Unit m(I i); } class C implements I { Unit m(I i) { if (i != null) skip; } }");

        ParamDecl p = met.getMethodSig().getParam(0);
        Block b = met.getBlock();

        IfStmt ifStmt = (IfStmt) b.getStmt(0);

        SimpleSet<VarOrFieldDecl> nonNull = ifStmt.getThen().nonNull_in();
        assertTrue(nonNull.contains(p));
    }

    @Test
    public void testMethodInvertedCondition() {
        MethodImpl met = getMethod("interface I { Unit m(I i); } class C implements I { Unit m(I i) { if (i == null) skip; else skip; } }");

        ParamDecl p = met.getMethodSig().getParam(0);
        Block b = met.getBlock();

        IfStmt ifStmt = (IfStmt) b.getStmt(0);

        SimpleSet<VarOrFieldDecl> nonNull = ifStmt.getElse().nonNull_in();
        assertTrue(nonNull.contains(p));
    }

    static private MethodImpl getMethod(String prog) {
        Model m = assertParse(prog);
        ClassDecl d = (ClassDecl) getTestModule(m).lookup(new KindedName(KindedName.Kind.CLASS, "UnitTest.C"));
        return d.getMethod(0);
    }

    static private ModuleDecl getTestModule(Model m) {
        ModuleDecl md = m.lookupModule("UnitTest");
        assertNotNull("Module UnitTest not found", md);
        return md;
    }
}
