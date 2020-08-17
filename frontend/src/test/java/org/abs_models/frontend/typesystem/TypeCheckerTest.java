/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.frontend.typesystem;

import static org.hamcrest.CoreMatchers.instanceOf;
import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.util.ArrayList;
import java.util.Collection;

import org.abs_models.ABSTest;
import org.abs_models.frontend.FrontendTest;
import org.abs_models.frontend.analyser.ErrorMessage;
import org.abs_models.frontend.analyser.SemanticConditionList;
import org.abs_models.frontend.ast.Block;
import org.abs_models.frontend.ast.ClassDecl;
import org.abs_models.frontend.ast.DataTypeUse;
import org.abs_models.frontend.ast.FieldUse;
import org.abs_models.frontend.ast.InterfaceDecl;
import org.abs_models.frontend.ast.InterfaceTypeUse;
import org.abs_models.frontend.ast.MethodImpl;
import org.abs_models.frontend.ast.MethodSig;
import org.abs_models.frontend.ast.Model;
import org.abs_models.frontend.ast.ModuleDecl;
import org.abs_models.frontend.ast.ParamDecl;
import org.abs_models.frontend.ast.ParametricDataTypeUse;
import org.abs_models.frontend.ast.ReturnStmt;
import org.abs_models.frontend.ast.TypeUse;
import org.abs_models.frontend.ast.VarDeclStmt;
import org.abs_models.frontend.ast.VarOrFieldUse;
import org.abs_models.frontend.ast.VarUse;
import org.abs_models.frontend.typechecker.InterfaceType;
import org.abs_models.frontend.typechecker.KindedName;
import org.junit.Assert;
import org.junit.Ignore;
import org.junit.Test;

public class TypeCheckerTest extends FrontendTest {

    // POSITIVE TESTS

    @Test
    @Ignore("https://github.com/abstools/abstools/issues/100")
    public void rosetreeTicket187() throws Exception {
        assertTypeCheckFileOk("abssamples/RoseTree.abs");
    }

    @Test
    public void subtypingTicket101() throws Exception {
        // https://github.com/abstools/abstools/issues/101
        assertTypeOK("interface I {} class A implements I {} { I a = new A(); List<I> afs = list[null, a]; }");
    }

    @Test
    public void tch_npe() throws Exception {
        assertTypeErrors("module Test; import ABS.StdLib.Bar; { ABS.StdLib.Bar x; }", Config.EXPECT_TYPE_ERROR);
    }

    @Test
    public void testVarDecl() {
        assertTypeOK("data Bool1 = True1 | False1; { Bool1 b = True1; }");
    }

    @Test
    public void testVarDeclInit() {
        assertTypeOK("interface I {} interface J extends I {} { J j; I i = j; }");
    }

    @Test
    public void fieldInit() {
        assertTypeOK("interface I {} class C implements I { I i = this; }");
    }

    @Test
    public void testClass() {
        assertTypeOK("interface I {} class C implements I {} { I i; i = new local C(); }");
    }

    @Test
    public void testClass2() {
        assertTypeOK("interface I {} interface J {} class C implements I,J {} { J j; j = new local C(); }");
    }

    @Test
    public void dataTypeTest() {
        assertTypeOK("data Foo = Bar; { Foo f = Bar; }");
    }

    @Test
    public void dataTypeParamTest() {
        assertTypeOK("data Nop = Nop; data Foo<X> = Bar(X); { Foo<Nop> f = Bar(Nop); }");
    }

    @Test
    public void test_dataTypeParam2base() {
        assertTypeOK("data AorB<A,B> = A(A) | B(B);");
    }

    @Test
    public void test_dataTypeParam2() {
        assertTypeOK("data AorB<A,B> = A(A getA) | B(B getB);");
    }

    @Test
    public void dataTypeSelectors() {
        assertTypeOK("data Foo = Bar(Foo foo) | Baz; { Foo b = foo(Bar(Baz)); }");
    }

    @Test
    public void dataTypeSelectors2() {
        assertTypeOK("data User = User(String login, String passwordHash);");
    }

    @Test
    public void dataTypeSelectorsParamType() {
        assertTypeOK("data User = User(List<String> names);");
    }

    @Test
    public void dataTypeSelectorsTypeParam() {
        assertTypeOK("data User<A> = User(List<A> names); { User<String> s = User(list[\"sdasd\"]); List<String> names = names(s); } ");
    }

    @Test
    public void dataTypeSelectorsTypeParam2() {
        assertTypeOK("data Foo<A> = Foo(A f);");
    }

    @Test
    public void negTestOk() {
        assertTypeOK("{ Bool b = !True; }");
    }

    @Test
    public void andOk() {
        assertTypeOK("{ Bool b = True && False;  }");
    }

    @Test
    public void plusOk() {
        assertTypeOK("{ Int i = 4 + 5; }");
        assertTypeOK("{ Float i = 4.0 + 5.0; }");
        assertTypeOK("{ Float i = 4.0e+08 + 5.0E-12; }");
    }

    @Test
    public void getOk() {
        assertTypeOK("{ Fut<Bool> f; Bool b = True; b = f.get; }");
    }

    @Test
    public void ticket414_futNeedsDataType1() {
        Model m = assertParse("module M; interface I {} { Fut<I> fi; }");
        assertFalse(m.hasErrors());
        Block b = m.getMainBlock();
        assertNotNull(b);
        VarDeclStmt s = (VarDeclStmt) b.getStmt(0);
        ParametricDataTypeUse u = (ParametricDataTypeUse) s.getVarDecl().getTypeUse();
        // Have:
        TypeUse tu = u.getParam(0);
        assertEquals("I",tu.getName());
        assertThat(tu, instanceOf(InterfaceTypeUse.class));
        assertThat(tu.getType(), instanceOf(InterfaceType.class));
        assertThat(tu.getDecl(), instanceOf(InterfaceDecl.class));
    }

    @Test
    public void ticket414_futNeedsDataType2() {
        Model m = assertParse("module M; data I = I; { Fut<I> fi; }");
        assertFalse(m.hasErrors());
        Block b = m.getMainBlock();
        assertNotNull(b);
        VarDeclStmt s = (VarDeclStmt) b.getStmt(0);
        ParametricDataTypeUse u = (ParametricDataTypeUse) s.getVarDecl().getTypeUse();
        // Have:
        TypeUse tu = u.getParam(0);
        assertEquals("I",tu.getName());
        assertThat(tu, instanceOf(DataTypeUse.class));
    }

    @Test
    public void letOk() {
        assertTypeOK("{ Bool b = let (Bool x) = True in x; }");
    }

    @Test
    public void caseOk() {
        assertTypeOK("{ Bool x = True; Bool b = case x { True => False; False => True; }; }");
    }

    @Test
    public void testWhenExp1() {
        assertTypeOK("def X frob<X>(X x) = x;");
    }

    @Test
    public void testWhenExp2() {
        assertTypeOK("def X frob<X>(X x) = when (True) then x else x;");
    }

    @Test
    public void caseVarOk() {
        assertTypeOK("data Foo = Bar(Bool); { Foo x = Bar(True);" + " Bool b = case x { Bar(y) => y; }; }");
    }

    @Test
    public void methodEmptyOk() {
        assertTypeOK("interface I { Unit m(); } class C implements I { Unit m() { } }");
    }

    @Test
    public void methodNoReturnOk() {
        assertTypeOK("interface I { Unit m(); } class C implements I { Unit m() { Bool b = True; b = False; } }");
    }

    @Test
    public void methodReturnOk() {
        assertTypeOK("interface I { Unit m(); } class C implements I { Unit m() { Bool b = True; return Unit; } }");
    }

    @Test
    public void methodParameterOk() {
        assertTypeOK("interface I { Bool m(Bool b);} " + "class C implements I { Bool m(Bool b) { return b; } }");
    }

    @Test
    public void methodInSuperType() {
        assertTypeOK("interface I { Bool m(Bool b);} interface J extends I { }"
                + "class C implements J { Bool m(Bool b) { J j; j = this; return j.m(True); } }");
    }

    @Test
    public void testIfOk() {
        assertTypeOK("{ if (True) { } else { } }");
    }

    @Test
    public void testWhileOk() {
        assertTypeOK("{ while (True) { } }");
    }

    @Test
    public void testAwaitClaimOk() {
        assertTypeOK("{ Fut<Bool> f; await f?; }");
    }

    @Test
    public void testAwaitBoolOk() {
        assertWarnings("{ Bool b = False; await b; }");
    }

    @Test
    public void testAwaitAndOk() {
        assertWarnings("{ await False && True; }");
    }

    @Test
    public void testAwaitTooPure1OK() {
        assertTypeOK("{ await timeValue(now()) > 0; }");
    }

    @Test
    public void testAwaitTooPure2OK() {
        // recursive def!
        assertTypeOK("{ await lookupDefault(insert(EmptyMap, Pair(1,timeValue(now()))),1,0) > 0; }");
    }

    @Test
    public void testAwaitTooPure3() {
        // recursive def!
        assertWarnings("{ await lookupDefault(insert(EmptyMap, Pair(1,timeValue(Time(0)))),1,0) > 0; }");
    }

    @Test
    public void syncCallMethodThis() {
        assertTypeOK("interface I { Unit m(); } " + "class C implements I { Unit m() { this.m(); } }");
    }

    @Test
    public void syncCallMethodThis2() {
        assertTypeOK("interface I { Unit m(); } interface J {}"
                + "class C implements J,I { Unit m() { this.m(); } }");
    }

    @Test
    public void syncCallMethodThis3() {
        assertTypeOK("interface I { Bool m(); } "
                + "class C implements I { Bool m() { Bool b = True; b = this.m(); return b; } }");
    }

    @Test
    public void syncCallMethodIntf() {
        assertTypeOK("interface I { Unit m(); } {I i; i.m(); }");
    }

    @Test
    public void syncCallThis() {
        assertTypeOK("class C { Unit m() { this.m(); } }");
    }

    @Test
    public void asyncCallMethodIntf() {
        assertTypeOK("interface I { Unit m(); } {I i; i!m(); }");
    }

    @Test
    public void fnAppTypeArgs() {
        assertTypeOK("def A f<A>(A a) = a; { Bool b = True; b = f(b); }");
    }

    @Test
    public void fnAppTypeArgs2() {
        assertTypeOK("def B optValue<B>(Maybe<B> val) = fromJust(val);");
    }

    @Test
    public void fnAppTypeArgs3() {
        assertTypeOK("def List<B> tail2<B>(List<B> list) = tail(list) ; ");
    }

    @Test
    public void fnAppTypeArgs4() {
        assertTypeOK("def B nth2<B>(List<B> list, Int n) = nth2(tail(list), n-1) ; ");
    }

    @Test
    public void fnAppTypeArgs5() {
        assertTypeOK("def List<B> shuffle<B>(List<B> list) = list;"
                + "def C chose<C>(List<C> list) = head(shuffle(list));");
    }

    @Test
    public void fnAppListUpgradableListType() {
        assertTypeOK("{ Set<Rat> s = set[4/3, 4]; }");
    }

    @Test
    public void fnAppListUpgradableListType2() {
        assertTypeOK("{ Set<Rat> s = set[4, 4/3]; }");
    }

    @Test
    public void constructorTypeArgs() {
        assertTypeOK("{ Maybe<Bool> o = Just(True); }");
    }

    @Test
    public void constructorTypeArgs2() {
        assertTypeOK("data Foo<A> = Bar(A,A); { Foo<Bool> o = Bar(True,True); }");
    }

    @Test
    public void constructorTypeArgs3() {
        assertTypeOK("data Foo<A,B> = Bar(A,B); { Foo<Bool,Int> o = Bar(True,5); }");
    }

    @Test
    public void constructorTypeArgs4() {
        assertTypeOK("{ Either<Int,Bool> o = Left(5); }");
    }

    @Test
    public void exception1() {
        assertTypeOK("exception E; { Exception e = E; }");
    }

    @Test
    public void exception2() {
        assertTypeOK("exception E(String); { Exception e = E(\"hello\"); }");
    }

    @Test
    public void exception3() {
        assertTypeOK("exception E(String); { String x = \"\"; try skip; catch E(p) => x = p; }");
    }

    @Test
    public void synonym1() {
        assertTypeOK("type A = Int; type B = A; { B b = 1; }");
    }

    @Test
    public void covariantTypeArgs() {
        assertTypeOK("interface I {} " +
                     "interface J extends I {} " +
                     "class C implements J {} " +
                     "{ I i = new local C(); " +
                     "  Maybe<I> o = Just(i); }");
    }

    @Test
    public void testListArgs() {
        assertTypeOK(" interface Database { } class DataBaseImpl(Map<String, List<String>> db) implements Database { } "
                + "{ Database db; db = new local DataBaseImpl(map[Pair(\"file0\", list[\"file\", \"from\", \"db\"])]); }");
    }

    @Test
    public void testMaybeDataType() {
        assertTypeOK("data MaybeTest<A> = NothingTest | JustTest(A);"
                + "def B fromJustTest<B>(MaybeTest<B> a) = case a { JustTest(j) => j; }; "
                + "{ Bool testresult = fromJustTest(JustTest(True)); }");
    }

    @Test
    public void patternMatching() {
        assertTypeOK("data List1<A> = Nil1 | Cons1(A, List1<A>); "
                + "data Pair1<A,B> = Pair1(A,B); data Server = SomeServer; def Server findServer(Server name, List1<Pair1<Server, Server>> list) ="
                + "case list { " + "Nil1 => SomeServer;" + "Cons1(Pair1(server, set), rest) => server; };");
    }

    @Test
    public void classParams() {
        assertTypeOK("interface I { Bool m(); } class C(Bool b) implements I { Bool m() { return b; } }");
    }

    @Test
    public void classParamsMethodShadowsField() {
        Model m = assertParse("class C(Bool b) { Bool m(Bool b) { return b; } }");
        ModuleDecl u = m.lookupModule("UnitTest");
        ClassDecl c = (ClassDecl) u.lookup(new KindedName(KindedName.Kind.CLASS, "C"));
        MethodImpl me = c.lookupMethod("m");
        ReturnStmt r = (ReturnStmt) me.getBlock().getStmt(0);
        VarOrFieldUse vu = (VarOrFieldUse) r.getRetExp();
        ParamDecl d = (ParamDecl) vu.getDecl();
        assertThat(d.getParent().getParent(), instanceOf(MethodSig.class));
        assertThat(vu.getClass().getName(), vu, instanceOf(VarUse.class));
    }

    @Test
    public void classParamsRewrite() {
        Model m = assertParse("class C(Bool b) { Bool m() { return b; } }");
        ModuleDecl u = m.lookupModule("UnitTest");
        ClassDecl c = (ClassDecl) u.lookup(new KindedName(KindedName.Kind.CLASS, "C"));
        MethodImpl me = c.lookupMethod("m");
        ReturnStmt r = (ReturnStmt) me.getBlock().getStmt(0);
        VarOrFieldUse vu = (VarOrFieldUse) r.getRetExp();
        ParamDecl d = (ParamDecl) vu.getDecl();
        assertThat(d.getParent().getParent(), instanceOf(ClassDecl.class));
        assertThat(vu.getClass().getName(), vu, instanceOf(FieldUse.class));
    }

    @Test
    public void classParamsRewrite2() {
        Model m = assertParse("class C(Bool b) { Bool m(Bool x) { return x; } }");
        ModuleDecl u = m.lookupModule("UnitTest");
        ClassDecl c = (ClassDecl) u.lookup(new KindedName(KindedName.Kind.CLASS, "C"));
        MethodImpl me = c.lookupMethod("m");
        ReturnStmt r = (ReturnStmt) me.getBlock().getStmt(0);
        VarOrFieldUse vu = (VarOrFieldUse) r.getRetExp();
        ParamDecl d = (ParamDecl) vu.getDecl();
        assertThat(d.getParent().getParent(), instanceOf(MethodSig.class));
        assertThat(vu.getClass().getName(), vu, instanceOf(VarUse.class));
    }

    @Test
    public void newExp() {
        assertTypeOK("class C(Bool b) { } { new local C(True); }");
    }

    @Test
    public void methodSigs() {
        Model m = assertParse("interface I { Unit m(); } interface J { Unit n(); } interface K extends I, J { Unit foo(); } { K k; } ");
        ModuleDecl module = m.lookupModule("UnitTest");
        InterfaceDecl d = (InterfaceDecl) module.getDecl(2);
        ArrayList<MethodSig> list = new ArrayList<>(d.getAllMethodSigs());
        assertEquals(list.toString(),3,list.size());

        VarDeclStmt stmt = (VarDeclStmt) module.getBlock().getStmt(0);
        Collection<MethodSig> sigs = stmt.getVarDecl().getTypeUse().getType().getAllMethodSigs();
        assertArrayEquals(sigs.toArray(),d.getAllMethodSigs().toArray());
    }

    @Test
    public void ticket256() {
        assertTypeOK("data D = Ticket256(Set<Int>);"
                + "def Set<Int> ticket256(D d) ="
                + "  case d {"
                + "    Ticket256(ds) => ds;"
                + "  };");
    }

    @Test
    public void ticket296() {
        assertTypeErrors("module FunArgsTypeCheckBug; def Int f(Map<Int,Int> m) = lookupDefault(m, 42);");
    }

    @Test
    public void test_Movecogto1() {
        Model m = assertParse("class C { Unit do() { movecogto 1; }}");
        SemanticConditionList errs = m.typeCheck();
        assertTrue(m.hasTypeErrors());
        Assert.assertEquals(ErrorMessage.EXPECTED_DC, errs.getFirstError().msg);
    }

    @Test
    public void deltaParseBS() throws Exception {
        String fileName = "abssamples/PVTest.abs";
        Model m = ABSTest.assertParseFileOk(fileName);
        if (m.hasParserErrors())
            fail(m.getParserErrors().get(0).toString());
    }
}
