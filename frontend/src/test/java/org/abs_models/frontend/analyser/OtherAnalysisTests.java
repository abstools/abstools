/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.frontend.analyser;

import static org.junit.Assert.*;
import static org.hamcrest.CoreMatchers.*;

import java.io.PrintWriter;
import java.io.StringWriter;

import org.abs_models.frontend.ast.ASTNode;
import org.abs_models.frontend.ast.Stmt;
import org.abs_models.frontend.tests.ABSFormatter;
import org.abs_models.frontend.tests.EmptyFormatter;
import org.abs_models.frontend.typechecker.DataTypeType;
import org.abs_models.frontend.typechecker.Type;
import org.junit.Test;

import org.abs_models.frontend.FrontendTest;
import org.abs_models.frontend.ast.AwaitAsyncCall;
import org.abs_models.frontend.ast.ClassDecl;
import org.abs_models.frontend.ast.DeltaDecl;
import org.abs_models.frontend.ast.Model;
import org.abs_models.frontend.ast.ReturnStmt;
import org.abs_models.frontend.ast.VarDeclStmt;

public class OtherAnalysisTests extends FrontendTest {

    @Test
    public void countCOG() {
        Model m = assertParseOk("interface I { } class C { { I i = new C(); } Unit m() { I i = new C(); } } { I i; i = new C(); i = new local C(); while (true) { i = new C(); }}");
        assertEquals(4, m.getNumberOfNewCogExpr());
    }

    @Test
    public void finalTest() {
        assertParse("interface I { } { [Final] I i; i = null; }", Config.TYPE_CHECK, Config.WITH_STD_LIB, Config.EXPECT_TYPE_ERROR);
    }

    @Test
    public void atomicTestFail1() {
        // await / get in atomic method
        assertParse("interface I {[Atomic] Unit n();} class C implements I {[Atomic] Unit n() {await this!n();}}",
                    Config.TYPE_CHECK, Config.WITH_STD_LIB, Config.EXPECT_TYPE_ERROR);
    }

    @Test
    public void atomicTestFail2() {
        // suspend in atomic method
        assertParse("interface I {[Atomic] Unit n();} class C implements I {[Atomic] Unit n() {suspend;}}",
                    Config.TYPE_CHECK, Config.WITH_STD_LIB, Config.EXPECT_TYPE_ERROR);
    }

    @Test
    public void atomicTestFail3() {
        // synccall to non-atomic method
        assertParse("interface I {Unit m(); [Atomic] Unit n();} class C implements I {Unit m() { skip; } [Atomic] Unit n() {this.m();}}",
                    Config.TYPE_CHECK, Config.WITH_STD_LIB, Config.EXPECT_TYPE_ERROR);
    }

    @Test
    public void atomicTestOk1() {
        // synccall to atomic method
        assertParse("interface I { [Atomic] Unit n();} class C implements I { [Atomic] Unit n() {this.n();}}",
                    Config.TYPE_CHECK, Config.WITH_STD_LIB);
    }

    @Test
    public void atomicTestOk2() {
        // non-synccall to non-atomic method
        assertParse("interface I {Unit m(); [Atomic] Unit n();} class C implements I {Unit m() { skip; } [Atomic] Unit n() {this!m();}}",
                    Config.TYPE_CHECK, Config.WITH_STD_LIB);
    }

    @Test
    public void fullcopyTest() {
        Model m = assertParseOk("module M; class C {}", Config.WITH_STD_LIB);
        Model m2 = m.treeCopyNoTransform();
        assertFalse(m.hasErrors());
        assertFalse(m2.hasErrors());
    }

    @Test
    public void fullcopyTest1() {
        Model m = assertParseOk("module M; class C {}", Config.WITH_STD_LIB);
        Model m2 = m.treeCopyNoTransform();
        assertFalse(m.hasErrors());
        assertFalse(m2.hasErrors());
        assertTrue(!m.typeCheck().containsErrors());
        // TODO try to reactivate this after fixing https://github.com/abstools/abstools/issues/239
        // assertTrue(!m2.typeCheck().containsErrors());
    }

    @Test
    public void fullcopyTest2() {
        Model m = assertParseOk("module M; class C {}", Config.WITH_STD_LIB);
        assertFalse(m.hasErrors());
        assertTrue(m.typeCheck().toString(),!m.typeCheck().containsErrors());
        Model m2 = m.treeCopyNoTransform();
        assertFalse(m2.hasErrors());
        // TODO try to reactivate this after fixing https://github.com/abstools/abstools/issues/239
        // assertTrue(m2.typeCheck().toString(),!m2.typeCheck().containsErrors());
    }

    public static String prettyPrint(Model m2) {
        StringWriter writer = new StringWriter();
        PrintWriter w = new PrintWriter(writer);
        ABSFormatter f = new EmptyFormatter();
        m2.doPrettyPrint(w,f);
        return writer.toString();
    }

    //@Test
    public void awaitTest2() {
        Model.doAACrewrite = true;
        // FIXME: the code in this example is incorrect (no await statement in init block allowed)
        Model m = assertParseOk("data Unit; interface I { Unit m(Unit x); } class C implements I {{Unit x = await this!m(Unit());}}");
        assertFalse(m.hasErrors());
        final String p1 = prettyPrint(m);
        Model.doAACrewrite = false;
        // Model m2 = assertParseOk("data Unit; interface I { Unit m(); } class C implements I {{Unit x = await this!m();}}");
        // FIXME: the code in this example is incorrect (no await statement in init block allowed)
        Model m2 = assertParseOk("data Unit; interface I { Unit m(Unit x); } class C implements I {{Unit x = await this!m(Unit());}}");
        assertFalse(m2.hasErrors());
        assertEquals(p1, prettyPrint(m2));
    }

    //@Test
    public void awaitTest3() {
        Model.doAACrewrite = true;
        // FIXME: the code in this example is incorrect (no await statement in init block allowed)
        Model m = assertParseOk("data Unit; interface I { Unit m(Unit x); } class C implements I {{Unit x = await this!n(Unit());}}");
        assertFalse(m.hasErrors());
        final String p1 = prettyPrint(m);
        Model.doAACrewrite = false;
        // FIXME: the code in this example is incorrect (no await statement in init block allowed)
        Model m2 = assertParseOk("data Unit; interface I { Unit m(Unit x); } class C implements I {{Unit x = await this!n(Unit());}}");
        assertFalse(m2.hasErrors());
        assertEquals(p1, prettyPrint(m2));
    }

    @Test
    public void testContext1() {
        // FIXME: the code in this example is incorrect (no await statement in init block allowed)
        Model m = assertParseOk("data Unit; interface I { Unit m(); } class C implements I {{Unit x = await this!m();}}");
        ClassDecl cd = (ClassDecl) m.lookupModule("UnitTest").getDecl(2);
        AwaitAsyncCall n = (AwaitAsyncCall) down(cd);
        assertNull("Rewrite failed!", n);
    }

    @Test
    public void testContext2() {
        // FIXME: the code in this example is incorrect (no await statement in init block allowed)
        Model m = assertParseOk("data Unit; interface I { Unit m(); } class C implements I {{Unit x = await this!m();}}");
        ClassDecl cd = (ClassDecl) m.lookupModule("UnitTest").getDecl(2);
        AwaitAsyncCall n = (AwaitAsyncCall) down(cd);
        assertNull("Rewriting failed!",n);
    }

    private static ASTNode<?> down(ASTNode<?> n) {
        ASTNode<?> x = null;
        for(int i =0; i<n.getNumChild(); i++) {
            x = n.getChild(i);
            if (x == null)
                continue;
            if (x instanceof AwaitAsyncCall)
                return x;
            else {
                x = down(x);
                if (x != null)
                    return x;
            }
        }
        return null;
    }

    @Test
    public void awaitRewriteModule1() {
        Model.doAACrewrite = false;
        Model m = assertParseOk("module A; export *; data X; module B; export *; data X; module C; import * from A; import B.X; class C { X m() { return await this!m();}}");
        ClassDecl c = (ClassDecl) m.lookupModule("C").getDecl(0);
        ReturnStmt ret = (ReturnStmt) c.getMethod(0).getBlock().getStmt(0);
        assertThat(ret.getRetExp().getType(), instanceOf(DataTypeType.class));
        assertEquals("A.X",ret.getRetExp().getType().getQualifiedName());
        Model.doAACrewrite = true;
        m = assertParseOk("module A; export *; data X; module B; export *; data X; module C; import * from A; import B.X; class C { X m() { return await this!m();}}", Config.WITH_STD_LIB);
        c = (ClassDecl) m.lookupModule("C").getDecl(0);
        Stmt s = c.getMethod(0).getBlock().getStmt(0);
        VarDeclStmt b = (VarDeclStmt) s;
        Type t = ((DataTypeType) b.getVarDecl().getType()).getTypeArg(0);
        assertEquals("A.X",t.getQualifiedName());
    }

    @Test
    public void awaitRewriteTwice() {
        assertTypeOK("module Test; interface I { I m(); } class C implements I { I m() { I x = await this!m(); await x!m(); return this; }}");
    }

    @Test
    public void awaitRewriteDecl1() {
        Model m = assertParseOk("module A; class C { } delta D; modifies class C { adds Unit m() { return await this!m();}}", Config.WITH_STD_LIB);
        DeltaDecl c = m.getDeltaDecls().iterator().next();
        AwaitAsyncCall a = (AwaitAsyncCall) down(c);
        assertNotNull(a); // pity, would like this to work.
    }

    @Test
    public void awaitRewriteDecl2() throws Exception {
        String deltaDecl = "delta D; modifies class C { adds Unit m() { return await this!m();}}";
        Model m = assertParseOk(deltaDecl);
        DeltaDecl d = m.findDelta("D");
        AwaitAsyncCall a = (AwaitAsyncCall) down(d);
        assertNotNull(a); // pity, would like this to work.
    }

    @Test
    public void foreachRewriteEmptyListLiteral() {
        assertTypeOK("module Test; { foreach (i in elements(set[])) { skip; } }");
    }
}
