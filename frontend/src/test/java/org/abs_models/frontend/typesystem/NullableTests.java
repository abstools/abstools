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
import org.junit.Test;

public class NullableTests extends FrontendTest {
    @Test
    public void test1() {
        Model m = assertParse("interface I { Unit m(I i); } class C implements I { Unit m(I i) { i.m(this); skip; } }");
        CompilationUnit cu = m.getCompilationUnit(0);
        ClassDecl d = (ClassDecl) getTestModule(m).lookup(new KindedName(KindedName.Kind.CLASS, "UnitTest.C"));
        MethodImpl met = d.getMethod(0);
        Block b = met.getBlock();
        System.out.println(b.getStmt(0).nonNull_in());
        System.out.println(b.getStmt(0).nonNull_out());
        System.out.println(b.getStmt(1).nonNull_out());
        assertTrue(true);
    }

    static private ModuleDecl getTestModule(Model m) {
        ModuleDecl md = m.lookupModule("UnitTest");
        assertNotNull("Module UnitTest not found", md);
        return md;
    }
}
