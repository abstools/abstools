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
