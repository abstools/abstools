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

public class ControlFlowTests extends FrontendTest {
    @Test
    public void simple() {
        MethodImpl met = getMethod("interface I { Unit m(I i); } class C implements I { Unit m(I i) { i = new C(); skip; suspend; } }");

        Block b = met.getBlock();

        CFGEntry entry = met.entry();
        CFGExit exit = met.exit();
        Stmt s0 = b.getStmt(0);
        Stmt s1 = b.getStmt(1);
        Stmt s2 = b.getStmt(2);

        assertEquals(1, b.pred().size());
        assertTrue(b.pred().contains(entry));
        assertEquals(1, b.succ().size());
        assertTrue(b.succ().contains(s0));

        assertEquals(1, s0.pred().size());
        assertTrue(s0.pred().contains(b));
        assertEquals(1, s0.succ().size());
        assertTrue(s0.succ().contains(s1));

        assertEquals(1, s1.pred().size());
        assertTrue(s1.pred().contains(s0));
        assertEquals(1, s1.succ().size());
        assertTrue(s1.succ().contains(s2));

        assertEquals(1, s2.pred().size());
        assertTrue(s2.pred().contains(s1));
        assertEquals(1, s2.succ().size());
        assertTrue(s2.succ().contains(exit));
    }

    @Test
    public void testAssert() {}

    @Test
    public void testAssign() {}

    @Test
    public void testCase() {}

    @Test
    public void testDuration() {}

    @Test
    public void testExpressionStmt() {}

    @Test
    public void testForEach() {}

    @Test
    public void testIf() {
        MethodImpl met = getMethod("interface I { Unit m(I i); } class C implements I { Unit m(I i) { if (i == null) skip; else suspend; } }");
        Block b = met.getBlock();

        IfStmt ifStmt = (IfStmt) b.getStmt(0);
        Block then = ifStmt.getThen();
        Block elseStmt = ifStmt.getElse();

        assertEquals(1, ifStmt.pred().size());
        assertEquals(2, ifStmt.succ().size());
        assertTrue(ifStmt.succ().contains(then));
        assertTrue(ifStmt.succ().contains(elseStmt));

        assertEquals(1, then.pred().size());
        assertTrue(then.pred().contains(ifStmt));
        assertEquals(1, then.succ().size());
        assertTrue(then.getStmt(0).succ().contains(met.exit()));

        assertEquals(1, elseStmt.pred().size());
        assertTrue(elseStmt.pred().contains(ifStmt));
        assertEquals(1, elseStmt.succ().size());
        assertTrue(elseStmt.getStmt(0).succ().contains(met.exit()));
    }

    @Test
    public void testMoveCogTo() {}

    @Test
    public void testThrow() {}

    @Test
    public void testTry() {}

    @Test
    public void testVarDecl() {}

    @Test
    public void testWhile() {}

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
