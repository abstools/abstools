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
    public void testAssert() {
        MethodImpl met = getMethod("interface I { Unit m(I i); } class C implements I { Unit m(I i) { assert i != null; skip; } }");
        Block b = met.getBlock();

        Stmt assertStmt = b.getStmt(0);
        Stmt skip = b.getStmt(1);
        Stmt exit = met.exit();

        assertEquals(2, assertStmt.succ().size());
        assertTrue(assertStmt.succ().contains(skip));
        assertTrue(assertStmt.succ().contains(exit));
    }

    @Test
    public void testAssign() {
        MethodImpl met = getMethod("interface I { Unit m(I i); } class C implements I { Unit m(I i) { Int n = 1 / 0; n = 0 / 1; skip; } }");
        Block b = met.getBlock();

        Stmt assignStmt1 = b.getStmt(0);
        Stmt assignStmt2 = b.getStmt(1);
        Stmt skip = b.getStmt(2);
        Stmt exit = met.exit();

        assertEquals(2, assignStmt1.succ().size());
        assertTrue(assignStmt1.succ().contains(assignStmt2));
        assertTrue(assignStmt1.succ().contains(exit));
        assertEquals(1, assignStmt2.succ().size());
        assertTrue(assignStmt2.succ().contains(skip));
    }

    @Test
    public void testCase() {
        MethodImpl met = getMethod("interface I { Unit m(I i); } class C implements I { Unit m(I i) { Pair<Int, Int> p = Pair(2, 3); Int x = 0; case p { Pair(2, y) => { x = y; skip; } Pair(3, y ) => skip; _ => x = -1; }  } }");
        Block b = met.getBlock();

        CaseStmt c = (CaseStmt) b.getStmt(2);
        CaseBranchStmt cb0 = c.getBranch(0);
        Block cbr0 = cb0.getRight();
        CaseBranchStmt cb1 = c.getBranch(1);
        Block cbr1 = cb1.getRight();
        CaseBranchStmt cb2 = c.getBranch(2);
        Block cbr2 = cb2.getRight();
        CFGExit exit = met.exit();

        assertEquals(1, c.succ().size());
        assertTrue(c.succ().contains(cb0));

        assertEquals(2, cb0.succ().size());
        assertTrue(cb0.succ().contains(cbr0));
        assertTrue(cb0.succ().contains(cb1));
        assertEquals(1, cbr0.succ().size());
        assertTrue(cbr0.succ().contains(cbr0.getStmt(0)));
        assertEquals(1, cbr0.getStmt(0).succ().size());
        assertTrue(cbr0.getStmt(0).succ().contains(cbr0.getStmt(1)));
        assertEquals(1, cbr0.getStmt(1).succ().size());
        assertTrue(cbr0.getStmt(1).succ().contains(exit));

        assertEquals(2, cb1.succ().size());
        assertTrue(cb1.succ().contains(cbr1));
        assertTrue(cb1.succ().contains(cb2));
        assertEquals(1, cbr1.succ().size());
        assertTrue(cbr1.succ().contains(cbr1.getStmt(0)));
        assertEquals(1, cbr1.getStmt(0).succ().size());
        assertTrue(cbr1.getStmt(0).succ().contains(exit));

        assertEquals(2, cb2.succ().size());
        assertTrue(cb2.succ().contains(cbr2));
        assertTrue(cb2.succ().contains(exit));
        assertEquals(1, cbr2.succ().size());
        assertTrue(cbr2.succ().contains(cbr2.getStmt(0)));
        assertEquals(1, cbr2.getStmt(0).succ().size());
        assertTrue(cbr1.getStmt(0).succ().contains(exit));
    }

    @Test
    public void testDuration() {
        MethodImpl met = getMethod("interface I { Unit m(Int i); } class C implements I { Unit m(Int i) { duration(1, 5); duration(case i { 0 => 10; 1 => 20; }, 500); skip; } }");
        Block b = met.getBlock();

        Stmt d0 = b.getStmt(0);
        Stmt d1 = b.getStmt(1);
        Stmt skip = b.getStmt(2);

        assertEquals(1, d0.succ().size());
        assertTrue(d0.succ().contains(d1));
        assertEquals(2, d1.succ().size());
        assertTrue(d1.succ().contains(skip));
        assertTrue(d1.succ().contains(met.exit()));
    }

    @Test
    public void testExpressionStmt_As1() {
        MethodImpl met = getMethod("interface I { Unit m(I i); } class C implements I { Unit m(I i) { i as I; skip; } }");
        Block b = met.getBlock();

        Stmt s = b.getStmt(0);
        Stmt skip = b.getStmt(1);

        assertEquals(1, s.succ().size());
        assertTrue(s.succ().contains(skip));
    }

    @Test
    public void testExpressionStmt_As2() {
        MethodImpl met = getMethod("interface I { Unit m(I i1, I i2, Int n); } class C implements I { Unit m(I i1, I i2, Int n) { case n { 0 => i1; 1 => i2; } as I; skip; } }");
        Block b = met.getBlock();

        Stmt s = b.getStmt(0);
        Stmt skip = b.getStmt(1);

        assertEquals(2, s.succ().size());
        assertTrue(s.succ().contains(skip));
        assertTrue(s.succ().contains(met.exit()));
    }

    @Test
    public void testExpressionStmt_Binary1() {
        MethodImpl met = getMethod("interface I { Unit m(I i1, I i2, Int n); } class C implements I { Unit m(I i1, I i2, Int n) { n + 1; skip; } }");
        Block b = met.getBlock();

        Stmt s = b.getStmt(0);
        Stmt skip = b.getStmt(1);

        assertEquals(1, s.succ().size());
        assertTrue(s.succ().contains(skip));
    }

    @Test
    public void testExpressionStmt_Binary2() {
        MethodImpl met = getMethod("interface I { Unit m(Int n1, Int n2); } class C implements I { Unit m(Int n1, Int n2) { case n1 { 0 => n1; 1 => n2; } * 2; skip; } }");
        Block b = met.getBlock();

        Stmt s = b.getStmt(0);
        Stmt skip = b.getStmt(1);

        assertEquals(2, s.succ().size());
        assertTrue(s.succ().contains(skip));
        assertTrue(s.succ().contains(met.exit()));
    }

    @Test
    public void testExpressionStmt_Binary3() {
        MethodImpl met = getMethod("interface I { Unit m(Int n1, Int n2); } class C implements I { Unit m(Int n1, Int n2) { 2 * case n1 { 0 => n1; 1 => n2; }; skip; } }");
        Block b = met.getBlock();

        Stmt s = b.getStmt(0);
        Stmt skip = b.getStmt(1);

        assertEquals(2, s.succ().size());
        assertTrue(s.succ().contains(skip));
        assertTrue(s.succ().contains(met.exit()));
    }

    @Test
    public void testExpressionStmt_BinaryDiv1() {
        MethodImpl met = getMethod("interface I { Unit m(Int n1, Int n2); } class C implements I { Unit m(Int n1, Int n2) { n1 / 500; skip; } }");
        Block b = met.getBlock();

        Stmt s = b.getStmt(0);
        Stmt skip = b.getStmt(1);

        assertEquals(1, s.succ().size());
        assertTrue(s.succ().contains(skip));
    }

    @Test
    public void testExpressionStmt_BinaryDiv2() {
        MethodImpl met = getMethod("interface I { Unit m(Int n1, Int n2); } class C implements I { Unit m(Int n1, Int n2) { n1 / 0; skip; } }");
        Block b = met.getBlock();

        Stmt s = b.getStmt(0);
        Stmt skip = b.getStmt(1);

        assertEquals(2, s.succ().size());
        assertTrue(s.succ().contains(skip));
        assertTrue(s.succ().contains(met.exit()));
    }

    @Test
    public void testExpressionStmt_BinaryDiv3() {
        MethodImpl met = getMethod("interface I { Unit m(Int n1, Int n2); } class C implements I { Unit m(Int n1, Int n2) { n1 / n2; skip; } }");
        Block b = met.getBlock();

        Stmt s = b.getStmt(0);
        Stmt skip = b.getStmt(1);

        assertEquals(2, s.succ().size());
        assertTrue(s.succ().contains(skip));
        assertTrue(s.succ().contains(met.exit()));
    }

    @Test
    public void testExpressionStmt_Case1() {
        MethodImpl met = getMethod("interface I { Unit m(Int n1, Int n2); } class C implements I { Unit m(Int n1, Int n2) { case n1 { 0 => n1; _ => n2 }; skip; } }");
        Block b = met.getBlock();

        ExpressionStmt s = (ExpressionStmt) b.getStmt(0);
        Stmt skip = b.getStmt(1);

        assertEquals(1, s.succ().size());
        assertTrue(s.succ().contains(skip));
    }

    @Test
    public void testExpressionStmt_Case2() {
        MethodImpl met = getMethod("interface I { Unit m(Int n1, Int n2); } class C implements I { Unit m(Int n1, Int n2) { case n1 { 0 => n1; 1 => n2 }; skip; } }");
        Block b = met.getBlock();

        ExpressionStmt s = (ExpressionStmt) b.getStmt(0);
        Stmt skip = b.getStmt(1);

        assertEquals(2, s.succ().size());
        assertTrue(s.succ().contains(skip));
        assertTrue(s.succ().contains(met.exit()));
    }

    @Test
    public void testExpressionStmt_DataConstructor1() {
        MethodImpl met = getMethod("interface I { Unit m(Int n1, Int n2); } class C implements I { Unit m(Int n1, Int n2) { Cons(1, Nil); skip; } }");
        Block b = met.getBlock();

        ExpressionStmt s = (ExpressionStmt) b.getStmt(0);
        Stmt skip = b.getStmt(1);

        assertEquals(1, s.succ().size());
        assertTrue(s.succ().contains(skip));
    }

    @Test
    public void testExpressionStmt_DataConstructor2() {
        MethodImpl met = getMethod("interface I { Unit m(Int n1, Int n2); } class C implements I { Unit m(Int n1, Int n2) { Cons(case n1 { 0 => n1; 1 => n2 }, Nil); skip; } }");
        Block b = met.getBlock();

        ExpressionStmt s = (ExpressionStmt) b.getStmt(0);
        Stmt skip = b.getStmt(1);

        assertEquals(2, s.succ().size());
        assertTrue(s.succ().contains(skip));
        assertTrue(s.succ().contains(met.exit()));
    }

    @Test
    public void testExpressionStmt_FnApp1() {
        MethodImpl met = getMethod("interface I { Unit m(Int n1, Int n2); } class C implements I { Unit m(Int n1, Int n2) { head(Cons(1, Nil)); skip; } }");
        Block b = met.getBlock();

        ExpressionStmt s = (ExpressionStmt) b.getStmt(0);
        Stmt skip = b.getStmt(1);

        assertEquals(1, s.succ().size());
        assertTrue(s.succ().contains(skip));
    }

    @Test
    public void testExpressionStmt_FnApp2() {
        MethodImpl met = getMethod("interface I { Unit m(Int n1, Int n2); } class C implements I { Unit m(Int n1, Int n2) { tail(case n1 { 0 => Cons(n1, Nil); 1 => Cons(n2, Nil); }); skip; } }");
        Block b = met.getBlock();

        ExpressionStmt s = (ExpressionStmt) b.getStmt(0);
        Stmt skip = b.getStmt(1);

        assertEquals(2, s.succ().size());
        assertTrue(s.succ().contains(skip));
        assertTrue(s.succ().contains(met.exit()));
    }

    @Test
    public void testExpressionStmt_If1() {
        MethodImpl met = getMethod("interface I { Unit m(Int n1, Int n2); } class C implements I { Unit m(Int n1, Int n2) { if n1 == 0 then n2 else n1; skip; } }");
        Block b = met.getBlock();

        ExpressionStmt s = (ExpressionStmt) b.getStmt(0);
        Stmt skip = b.getStmt(1);

        assertEquals(1, s.succ().size());
        assertTrue(s.succ().contains(skip));
    }

    @Test
    public void testExpressionStmt_If2() {
        MethodImpl met = getMethod("interface I { Unit m(Int n1, Int n2); } class C implements I { Unit m(Int n1, Int n2) { if n1 / n2 == 0 then n2 else n1; skip; } }");
        Block b = met.getBlock();

        ExpressionStmt s = (ExpressionStmt) b.getStmt(0);
        Stmt skip = b.getStmt(1);

        assertEquals(2, s.succ().size());
        assertTrue(s.succ().contains(skip));
        assertTrue(s.succ().contains(met.exit()));
    }

    @Test
    public void testExpressionStmt_If3() {
        MethodImpl met = getMethod("interface I { Unit m(Int n1, Int n2); } class C implements I { Unit m(Int n1, Int n2) { if n1 == 0 then n2 / n2 else n1; skip; } }");
        Block b = met.getBlock();

        ExpressionStmt s = (ExpressionStmt) b.getStmt(0);
        Stmt skip = b.getStmt(1);

        assertEquals(2, s.succ().size());
        assertTrue(s.succ().contains(skip));
        assertTrue(s.succ().contains(met.exit()));
    }

    @Test
    public void testExpressionStmt_If4() {
        MethodImpl met = getMethod("interface I { Unit m(Int n1, Int n2); } class C implements I { Unit m(Int n1, Int n2) { if n1 == 0 then n2 else n1 / n2; skip; } }");
        Block b = met.getBlock();

        ExpressionStmt s = (ExpressionStmt) b.getStmt(0);
        Stmt skip = b.getStmt(1);

        assertEquals(2, s.succ().size());
        assertTrue(s.succ().contains(skip));
        assertTrue(s.succ().contains(met.exit()));
    }

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
