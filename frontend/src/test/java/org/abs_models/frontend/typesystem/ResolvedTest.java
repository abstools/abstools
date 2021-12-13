package org.abs_models.frontend.typesystem;

import org.abs_models.frontend.FrontendTest;
import org.abs_models.frontend.ast.*;
import org.abs_models.frontend.typechecker.KindedName;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

public class ResolvedTest extends FrontendTest {

    @Test
    public void asyncCallGet() {
        Model m = assertParse("interface I { Int m(); } class C implements I { Int m() { return 4; } } { I i = new C(); Fut<Int> f = i!m(); Int n = f.get; }");
        MainBlock mb = m.getMainBlock();

        Stmt s0 = mb.getStmt(0);
        Stmt s1 = mb.getStmt(1);
        Stmt s2 = mb.getStmt(2);
        VarDecl d = ((VarDeclStmt) s1).getVarDecl();

        assertEquals(0, s0.resolved_in().size());
        assertEquals(0, s0.resolved_out().size());

        assertEquals(0, s1.resolved_in().size());
        assertEquals(0, s1.resolved_out().size());

        assertEquals(0, s2.resolved_in().size());
        assertEquals(1, s2.resolved_out().size());
        assertTrue(s2.resolved_out().contains(d));
    }

    @Test
    public void asyncCallAwait() {
        Model m = assertParse("interface I { Int m(); } class C implements I { Int m() { return 4; } } { I i = new C(); Fut<Int> f = i!m(); await f?; }");
        MainBlock mb = m.getMainBlock();

        Stmt s0 = mb.getStmt(0);
        Stmt s1 = mb.getStmt(1);
        Stmt s2 = mb.getStmt(2);
        VarDecl d = ((VarDeclStmt) s1).getVarDecl();

        assertEquals(0, s0.resolved_in().size());
        assertEquals(0, s0.resolved_out().size());

        assertEquals(0, s1.resolved_in().size());
        assertEquals(0, s1.resolved_out().size());

        assertEquals(0, s2.resolved_in().size());
        assertEquals(1, s2.resolved_out().size());
        assertTrue(s2.resolved_out().contains(d));
    }

    @Test
    public void ifExp() {
        Model m = assertParse("interface I { Int m(); } class C implements I { Int m() { return 4; } } { Int n = 1; I i = new C(); Fut<Int> f1 = i!m(); Fut<Int> f2 = i!m(); await f1?; f2.get; Fut<Int> f3 = when n == 0 then f1 else f2; }");
        MainBlock mb = m.getMainBlock();

        Stmt s0 = mb.getStmt(0);
        Stmt s1 = mb.getStmt(1);
        Stmt s2 = mb.getStmt(2);
        Stmt s3 = mb.getStmt(3);
        Stmt s4 = mb.getStmt(4);
        Stmt s5 = mb.getStmt(5);
        Stmt s6 = mb.getStmt(6);

        VarDecl d0 = ((VarDeclStmt) s2).getVarDecl();
        VarDecl d1 = ((VarDeclStmt) s3).getVarDecl();
        VarDecl d2 = ((VarDeclStmt) s6).getVarDecl();

        assertEquals(0, s0.resolved_in().size());
        assertEquals(0, s0.resolved_out().size());

        assertEquals(0, s1.resolved_in().size());
        assertEquals(0, s1.resolved_out().size());

        assertEquals(0, s2.resolved_in().size());
        assertEquals(0, s2.resolved_out().size());

        assertEquals(0, s3.resolved_in().size());
        assertEquals(0, s3.resolved_out().size());

        assertEquals(0, s4.resolved_in().size());
        assertEquals(1, s4.resolved_out().size());
        assertTrue(s4.resolved_out().contains(d0));

        assertEquals(1, s5.resolved_in().size());
        assertTrue(s5.resolved_in().contains(d0));
        assertEquals(2, s5.resolved_out().size());
        assertTrue(s5.resolved_out().contains(d0));
        assertTrue(s5.resolved_out().contains(d1));

        assertEquals(2, s6.resolved_in().size());
        assertTrue(s6.resolved_in().contains(d0));
        assertTrue(s6.resolved_in().contains(d1));
        assertEquals(3, s6.resolved_out().size());
        assertTrue(s6.resolved_out().contains(d0));
        assertTrue(s6.resolved_out().contains(d1));
        assertTrue(s6.resolved_out().contains(d2));
    }

    @Test
    public void caseExp() {
        Model m = assertParse("interface I { Int m(); } class C implements I { Int m() { return 4; } } { Int n = 1; I i = new C(); Fut<Int> f1 = i!m(); Fut<Int> f2 = i!m(); await f1?; f2.get; Fut<Int> f3 = case n { 0 => f1; 1 => f2; }; }");
        MainBlock mb = m.getMainBlock();

        Stmt s0 = mb.getStmt(0);
        Stmt s1 = mb.getStmt(1);
        Stmt s2 = mb.getStmt(2);
        Stmt s3 = mb.getStmt(3);
        Stmt s4 = mb.getStmt(4);
        Stmt s5 = mb.getStmt(5);
        Stmt s6 = mb.getStmt(6);

        VarDecl d0 = ((VarDeclStmt) s2).getVarDecl();
        VarDecl d1 = ((VarDeclStmt) s3).getVarDecl();
        VarDecl d2 = ((VarDeclStmt) s6).getVarDecl();

        assertEquals(0, s0.resolved_in().size());
        assertEquals(0, s0.resolved_out().size());

        assertEquals(0, s1.resolved_in().size());
        assertEquals(0, s1.resolved_out().size());

        assertEquals(0, s2.resolved_in().size());
        assertEquals(0, s2.resolved_out().size());

        assertEquals(0, s3.resolved_in().size());
        assertEquals(0, s3.resolved_out().size());

        assertEquals(0, s4.resolved_in().size());
        assertEquals(1, s4.resolved_out().size());
        assertTrue(s4.resolved_out().contains(d0));

        assertEquals(1, s5.resolved_in().size());
        assertTrue(s5.resolved_in().contains(d0));
        assertEquals(2, s5.resolved_out().size());
        assertTrue(s5.resolved_out().contains(d0));
        assertTrue(s5.resolved_out().contains(d1));

        assertEquals(2, s6.resolved_in().size());
        assertTrue(s6.resolved_in().contains(d0));
        assertTrue(s6.resolved_in().contains(d1));
        assertEquals(3, s6.resolved_out().size());
        assertTrue(s6.resolved_out().contains(d0));
        assertTrue(s6.resolved_out().contains(d1));
        assertTrue(s6.resolved_out().contains(d2));
    }

    @Test
    public void whileStmt() {
        Model m = assertParse("interface I { Int m(); } class C implements I { Int m() { return 4; } } { Int i = 4; I i = new C(); Fut<Int> f = i!m(); Int n = f.get; while (i > 0) { i = i - 1; } skip; }");
        MainBlock mb = m.getMainBlock();

        Stmt s0 = mb.getStmt(0);
        Stmt s1 = mb.getStmt(1);
        Stmt s2 = mb.getStmt(2);
        Stmt s3 = mb.getStmt(3);
        WhileStmt s4 = (WhileStmt) mb.getStmt(4);
        Stmt s5 = mb.getStmt(5);
        VarDecl d = ((VarDeclStmt) s2).getVarDecl();

        assertEquals(0, s0.resolved_in().size());
        assertEquals(0, s0.resolved_out().size());

        assertEquals(0, s1.resolved_in().size());
        assertEquals(0, s1.resolved_out().size());

        assertEquals(0, s2.resolved_in().size());
        assertEquals(0, s2.resolved_out().size());

        assertEquals(0, s3.resolved_in().size());
        assertEquals(1, s3.resolved_out().size());
        assertTrue(s3.resolved_out().contains(d));

        assertEquals(1, s4.resolved_in().size());
        assertEquals(1, s4.resolved_out().size());
        assertTrue(s4.resolved_out().contains(d));

        assertEquals(1, s4.getBody().getStmt(0).resolved_in().size());
        assertEquals(1, s4.getBody().getStmt(0).resolved_out().size());
        assertTrue(s4.getBody().getStmt(0).resolved_out().contains(d));

        assertEquals(1, s5.resolved_in().size());
        assertEquals(1, s5.resolved_out().size());
        assertTrue(s5.resolved_out().contains(d));
    }

    static private ClassDecl getClass(String prog) {
        Model m = assertParse(prog);
        return (ClassDecl) getTestModule(m).lookup(new KindedName(KindedName.Kind.CLASS, "UnitTest.C"));
    }

    static private MethodImpl getMethod(String prog) {
        return getClass(prog).getMethod(0);
    }

    static private ModuleDecl getTestModule(Model m) {
        ModuleDecl md = m.lookupModule("UnitTest");
        assertNotNull("Module UnitTest not found", md);
        return md;
    }
}
