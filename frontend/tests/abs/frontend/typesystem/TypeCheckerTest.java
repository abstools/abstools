/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.typesystem;

import static org.junit.Assert.*;

import java.util.ArrayList;
import java.util.Collection;

import org.junit.Test;

import abs.frontend.FrontendTest;
import abs.frontend.ast.InterfaceDecl;
import abs.frontend.ast.MethodSig;
import abs.frontend.ast.Model;
import abs.frontend.ast.ModuleDecl;
import abs.frontend.ast.VarDeclStmt;

public class TypeCheckerTest extends FrontendTest {

    // POSITIVE TESTS

    @Test
    public void abslang() throws Exception {
        assertTypeCheckFileOk("src/abs/lang/abslang.abs", false);
    }

    @Test
    public void lizeth() throws Exception {
        assertTypeCheckFileOk("tests/abssamples/lizeth.abs", false);
    }

    @Test
    public void testVarDecl() {
        assertNoTypeErrorsNoLib("data Bool = True | False; { Bool b = True; }");
    }

    @Test
    public void testVarDeclInit() {
        assertNoTypeErrorsNoLib("interface I {} interface J extends I {} { J j; I i = j; }");
    }

    @Test
    public void fieldInit() {
        assertNoTypeErrorsNoLib("interface I {} class C implements I { I i = this; }");
    }
    
    @Test
    public void testClass() {
        assertNoTypeErrorsNoLib("interface I {} class C implements I {} { I i; i = new C(); }");
    }

    @Test
    public void testClass2() {
        assertNoTypeErrorsNoLib("interface I {} interface J {} class C implements I,J {} { J j; j = new C(); }");
    }

    @Test
    public void dataTypeTest() {
        assertNoTypeErrorsNoLib("data Foo = Bar; { Foo f = Bar; }");
    }

    @Test
    public void dataTypeParamTest() {
        assertNoTypeErrorsNoLib("data Nop = Nop; data Foo<X> = Bar(X); { Foo<Nop> f = Bar(Nop); }");
    }

    @Test
    public void dataTypeSelectors() {
        assertNoTypeErrorsNoLib("data Foo = Bar(Foo foo) | Baz; { Foo b = foo(Bar(Baz)); }");
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
        assertTypeOK("{ Bool b = ~True; }");
    }

    @Test
    public void andOk() {
        assertTypeOK("{ Bool b = True && False;  }");
    }

    @Test
    public void plusOk() {
        assertTypeOK("{ Int i = 4 + 5; }");
    }

    @Test
    public void getOk() {
        assertTypeOK("{ Fut<Bool> f; Bool b = True; b = f.get; }");
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
        assertTypeErrors("{ Bool b = False; await b; }");
    }

    @Test
    public void testAwaitAndOk() {
        assertTypeErrors("{ await False && True; }");
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
        assertTypeErrors("{ await lookupDefault(insert(EmptyMap, Pair(1,timeValue(Time(0)))),1,0) > 0; }");
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
        assertTypeOK("def B nth<B>(List<B> list, Int n) = nth(tail(list), n-1) ; ");
    }

    @Test
    public void fnAppTypeArgs5() {
        assertTypeOK("def List<B> shuffle<B>(List<B> list) = list;"
                + "def C chose<C>(List<C> list) = head(shuffle(list));");
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
    public void covariantTypeArgs() {
        assertTypeOK("interface I {} " +
                     "interface J extends I {} " +
                     "class C implements J {} " +
                     "{ I i = new C(); " +
                     "  Maybe<I> o = Just(i); }");
    }
    
    @Test
    public void testListArgs() {
        assertTypeOK(" interface Database { } class DataBaseImpl(Map<String, List<String>> db) implements Database { } "
                + "{ Database db; db = new DataBaseImpl(map[Pair(\"file0\", list[\"file\", \"from\", \"db\"])]); }");
    }

    @Test
    public void testMaybeDataType() {
        assertTypeOK("data MaybeTest<A> = NothingTest | JustTest(A);"
                + "def B fromJustTest<B>(MaybeTest<B> a) = case a { JustTest(j) => j; }; "
                + "{ Bool testresult = fromJustTest(JustTest(True)); }");
    }

    @Test
    public void patternMatching() {
        assertNoTypeErrorsNoLib("data List<A> = Nil | Cons(A, List<A>); "
                + "data Pair<A,B> = Pair(A,B); data Server = SomeServer; def Server findServer(Server name, List<Pair<Server, Server>> list) ="
                + "case list { " + "Nil => SomeServer;" + "Cons(Pair(server, set), rest) => server; };");
    }

    @Test
    public void classParams() {
        assertTypeOK("interface I { Bool m(); } class C(Bool b) implements I { Bool m() { return b; } }");
    }

    @Test
    public void newExp() {
        assertTypeOK("class C(Bool b) { } { new C(True); }");
    }
    
    @Test
    public void methodSigs() {
        Model m = assertParseOk("interface I { Unit m(); } interface J { Unit n(); } interface K extends I, J { Unit foo(); } { K k; } ", Config.WITH_STD_LIB); 
        ModuleDecl module = m.getCompilationUnit(1).getModuleDecl(0);
        InterfaceDecl d = (InterfaceDecl) module.getDecl(2);
        ArrayList<MethodSig> list = new ArrayList<MethodSig>(d.getAllMethodSigs());
        assertEquals(list.toString(),3,list.size());
        
        VarDeclStmt stmt = (VarDeclStmt) module.getBlock().getStmt(0);
        Collection<MethodSig> sigs = stmt.getVarDecl().getAccess().getType().getAllMethodSigs();
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
}
