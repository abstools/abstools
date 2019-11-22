/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.frontend.analyser;

import static org.hamcrest.CoreMatchers.instanceOf;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;

import org.abs_models.backend.common.InternalBackendException;
import org.abs_models.backend.prettyprint.ABSFormatter;
import org.abs_models.backend.prettyprint.DefaultABSFormatter;
import org.abs_models.common.WrongProgramArgumentException;
import org.abs_models.frontend.FrontendTest;
import org.abs_models.frontend.ast.ASTNode;
import org.abs_models.frontend.ast.AwaitAsyncCall;
import org.abs_models.frontend.ast.ClassDecl;
import org.abs_models.frontend.ast.DeltaDecl;
import org.abs_models.frontend.ast.Model;
import org.abs_models.frontend.ast.ReturnStmt;
import org.abs_models.frontend.ast.Stmt;
import org.abs_models.frontend.ast.VarDeclStmt;
import org.abs_models.frontend.typechecker.DataTypeType;
import org.abs_models.frontend.typechecker.Type;
import org.junit.Test;

public class OtherAnalysisTests extends FrontendTest {

    @Test
    public void countCOG() {
        Model m = assertParse("interface I { } class C { { I i = new C(); } Unit m() { I i = new C(); } } { I i; i = new C(); i = new local C(); while (true) { i = new C(); }}");
        // 4 from the sample code, 1 from the standard library
        assertEquals(5, m.getNumberOfNewCogExpr());
    }

    @Test
    public void finalTest() {
        assertParse("interface I { } { [Final] I i; i = null; }", Config.TYPE_CHECK, Config.EXPECT_TYPE_ERROR);
    }

    @Test
    public void atomicTestFail1() {
        // await / get in atomic method
        assertParse("interface I {[Atomic] Unit n();} class C implements I {[Atomic] Unit n() {await this!n();}}",
                    Config.TYPE_CHECK, Config.EXPECT_TYPE_ERROR);
    }

    @Test
    public void atomicTestFail2() {
        // suspend in atomic method
        assertParse("interface I {[Atomic] Unit n();} class C implements I {[Atomic] Unit n() {suspend;}}",
                    Config.TYPE_CHECK, Config.EXPECT_TYPE_ERROR);
    }

    @Test
    public void atomicTestFail3() {
        // synccall to non-atomic method
        assertParse("interface I {Unit m(); [Atomic] Unit n();} class C implements I {Unit m() { skip; } [Atomic] Unit n() {this.m();}}",
                    Config.TYPE_CHECK, Config.EXPECT_TYPE_ERROR);
    }

    @Test
    public void atomicTestOk1() {
        // synccall to atomic method
        assertParse("interface I { [Atomic] Unit n();} class C implements I { [Atomic] Unit n() {this.n();}}",
                    Config.TYPE_CHECK);
    }

    @Test
    public void atomicTestOk2() {
        // non-synccall to non-atomic method
        assertParse("interface I {Unit m(); [Atomic] Unit n();} class C implements I {Unit m() { skip; } [Atomic] Unit n() {this!m();}}",
                    Config.TYPE_CHECK);
    }

    @Test
    public void fullcopyTest() {
        Model m = assertParse("module M; class C {}");
        Model m2 = m.treeCopyNoTransform();
        assertFalse(m.hasErrors());
        assertFalse(m2.hasErrors());
    }

    @Test
    public void fullcopyTest1() {
        Model m = assertParse("module M; class C {}");
        Model m2 = m.treeCopyNoTransform();
        assertFalse(m.hasErrors());
        assertFalse(m2.hasErrors());
        assertTrue(!m.typeCheck().containsErrors());
        // TODO try to reactivate this after fixing https://github.com/abstools/abstools/issues/239
        // assertTrue(!m2.typeCheck().containsErrors());
    }

    @Test
    public void fullcopyTest2() {
        Model m = assertParse("module M; class C {}");
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
        ABSFormatter f = new DefaultABSFormatter(w);
        m2.doPrettyPrint(w,f);
        return writer.toString();
    }

    //@Test
    public void awaitTest2() {
        // FIXME: the code in this example is incorrect (no await statement in init block allowed)
        Model m = assertParse("data Unit; interface I { Unit m(Unit x); } class C implements I {{Unit x = await this!m(Unit());}}");
        assertFalse(m.hasErrors());
        final String p1 = prettyPrint(m);
        // Model m2 = assertParseOk("data Unit; interface I { Unit m(); } class C implements I {{Unit x = await this!m();}}");
        // FIXME: the code in this example is incorrect (no await statement in init block allowed)
        Model m2 = assertParse("data Unit; interface I { Unit m(Unit x); } class C implements I {{Unit x = await this!m(Unit());}}",
                               Config.WITHOUT_DESUGARING_AFTER_TYPECHECK);
        assertFalse(m2.hasErrors());
        assertEquals(p1, prettyPrint(m2));
    }

    //@Test
    public void awaitTest3() {
        // FIXME: the code in this example is incorrect (no await statement in init block allowed)
        Model m = assertParse("data Unit; interface I { Unit m(Unit x); } class C implements I {{Unit x = await this!n(Unit());}}");
        assertFalse(m.hasErrors());
        final String p1 = prettyPrint(m);
        // FIXME: the code in this example is incorrect (no await statement in init block allowed)
        Model m2 = assertParse("data Unit; interface I { Unit m(Unit x); } class C implements I {{Unit x = await this!n(Unit());}}",
                               Config.WITHOUT_DESUGARING_AFTER_TYPECHECK);
        assertFalse(m2.hasErrors());
        assertEquals(p1, prettyPrint(m2));
    }

    @Test
    public void testContext1() {
        Model m = assertParse("interface I { Unit m(); } class C implements I { Unit m() {Unit x = await this!m();}}",
            Config.TYPE_CHECK);
        ClassDecl cd = (ClassDecl) m.lookupModule("UnitTest").getDecl(1);
        int n =  cd.findChildren(AwaitAsyncCall.class).size();
        assertEquals("Rewrite failed", n, 0);
    }

    @Test
    public void awaitRewriteModule1() {
        Model m = assertParse("module A; export *; data X; module B; export *; data X; module C; import * from A; import B.X; class C { X m() { return await this!m();}}",
            Config.TYPE_CHECK, Config.WITHOUT_DESUGARING_AFTER_TYPECHECK);
        ClassDecl c = (ClassDecl) m.lookupModule("C").getDecl(0);
        Stmt stmt = c.getMethod(0).getBlock().getStmt(0);
        ReturnStmt ret = (ReturnStmt) stmt;
        assertThat(ret.getRetExp().getType(), instanceOf(DataTypeType.class));
        assertEquals("A.X",ret.getRetExp().getType().getQualifiedName());
        m = assertParse("module A; export *; data X; module B; export *; data X; module C; import * from A; import B.X; class C { X m() { return await this!m();}}",
            Config.TYPE_CHECK);
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
        Model m = assertParse("module A; class C { } delta D; modifies class C { adds Unit m() { return await this!m();}}");
        DeltaDecl c = m.getDeltaDecls().iterator().next();
        // pity, would like this to work so that size == 0
        assertEquals(c.findChildren(AwaitAsyncCall.class).size(), 1);
    }

    @Test
    public void awaitRewriteDecl2() throws Exception {
        String deltaDecl = "delta D; modifies class C { adds Unit m() { return await this!m();}}";
        Model m = assertParse(deltaDecl);
        DeltaDecl d = m.findDelta("D");
        // pity, would like this to work so that size == 0
        assertEquals(d.findChildren(AwaitAsyncCall.class).size(), 1);
    }

    @Test
    public void foreachRewriteEmptyListLiteral() {
        assertTypeOK("module Test; { foreach (i in elements(set[])) { skip; } }");
    }

    @Test
    public void bug184() throws WrongProgramArgumentException, InternalBackendException, IOException {
        assertParseFileOk("abssamples/frontend/bug184.abs", Config.TYPE_CHECK);
    }
}
