package abs.frontend.typesystem;

import static org.junit.Assert.*;

import org.junit.Test;

import abs.frontend.FrontendTest;
import abs.frontend.analyser.ErrorMessage;
import abs.frontend.analyser.SemanticErrorList;
import abs.frontend.ast.Model;

public class TypeCheckerTest extends FrontendTest {

    // POSITIVE TESTS

    @Test
    public void testVarDecl() {
        assertNoTypeErrorsNoLib("data Bool = True | False; { Bool b = True; }");
    }

    @Test
    public void testVarDeclInit() {
        assertNoTypeErrorsNoLib("interface I {} interface J extends I {} { J j; I i = j; }");
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
    public void negTestOk() {
        assertNoTypeErrors("{ Bool b = ~True; }");
    }

    @Test
    public void andOk() {
        assertNoTypeErrors("{ Bool b = True && False;  }");
    }

    @Test
    public void plusOk() {
        assertNoTypeErrors("{ Int i = 4 + 5; }");
    }

    @Test
    public void getOk() {
        assertNoTypeErrors("{ Fut<Bool> f; Bool b = True; b = f.get; }");
    }

    @Test
    public void letOk() {
        assertNoTypeErrors("{ Bool b = let (Bool x) = True in x; }");
    }

    @Test
    public void caseOk() {
        assertNoTypeErrors("{ Bool x = True; Bool b = case x { True => False; False => True; }; }");
    }

    @Test
    public void caseVarOk() {
        assertNoTypeErrors("data Foo = Bar(Bool); { Foo x = Bar(True);" + " Bool b = case x { Bar(y) => y; }; }");
    }

    @Test
    public void methodEmptyOk() {
        assertNoTypeErrors("interface I { Unit m(); } class C implements I { Unit m() { } }");
    }

    @Test
    public void methodNoReturnOk() {
        assertNoTypeErrors("interface I { Unit m(); } class C implements I { Unit m() { Bool b = True; b = False; } }");
    }

    @Test
    public void methodReturnOk() {
        assertNoTypeErrors("interface I { Unit m(); } class C implements I { Unit m() { Bool b = True; return Unit; } }");
    }

    @Test
    public void methodParameterOk() {
        assertNoTypeErrors("interface I { Bool m(Bool b);} " + "class C implements I { Bool m(Bool b) { return b; } }");
    }

    @Test
    public void methodInSuperType() {
        assertNoTypeErrors("interface I { Bool m(Bool b);} interface J extends I { }"
                + "class C implements J { Bool m(Bool b) { J j; j = this; return j.m(True); } }");

    }

    @Test
    public void testIfOk() {
        assertNoTypeErrors("{ if (True) { } else { } }");
    }

    @Test
    public void testWhileOk() {
        assertNoTypeErrors("{ while (True) { } }");
    }

    @Test
    public void testAwaitClaimOk() {
        assertNoTypeErrors("{ Fut<Bool> f; await f?; }");
    }

    @Test
    public void testAwaitBoolOk() {
        assertNoTypeErrors("{ Bool b = False; await b; }");
    }

    @Test
    public void testAwaitAndOk() {
        assertNoTypeErrors("{ await False && True; }");
    }

    @Test
    public void syncCallMethodThis() {
        assertNoTypeErrors("interface I { Unit m(); } " + "class C implements I { Unit m() { this.m(); } }");
    }

    @Test
    public void syncCallMethodThis2() {
        assertNoTypeErrors("interface I { Unit m(); } interface J {}"
                + "class C implements J,I { Unit m() { this.m(); } }");
    }

    @Test
    public void syncCallMethodThis3() {
        assertNoTypeErrors("interface I { Bool m(); } "
                + "class C implements I { Bool m() { Bool b = True; b = this.m(); return b; } }");
    }

    @Test
    public void syncCallMethodThisInitBlock() {
        assertNoTypeErrors("interface I { Bool m(); } "
                + "class C implements I { { Bool b = True; b = this.m(); } Bool m() { return True; } }");
    }

    @Test
    public void syncCallMethodIntf() {
        assertNoTypeErrors("interface I { Unit m(); } {I i; i.m(); }");
    }

    @Test
    public void syncCallThis() {
        assertNoTypeErrors("class C { Unit m() { this.m(); } }");
    }

    @Test
    public void asyncCallMethodIntf() {
        assertNoTypeErrors("interface I { Unit m(); } {I i; i!m(); }");
    }

    @Test
    public void fnAppTypeArgs() {
        assertNoTypeErrors("def A f<A>(A a) = a; { Bool b = True; b = f(b); }");
    }

    @Test
    public void fnAppTypeArgs2() {
        assertNoTypeErrors("def B optValue<B>(Maybe<B> val) = fromJust(val);");
    }

    @Test
    public void fnAppTypeArgs3() {
        assertNoTypeErrors("def List<B> tail2<B>(List<B> list) = tail(list) ; ");
    }

    @Test
    public void fnAppTypeArgs4() {
        assertNoTypeErrors("def B nth<B>(List<B> list, Int n) = nth(tail(list), n-1) ; ");
    }

    @Test
    public void fnAppTypeArgs5() {
        assertNoTypeErrors("def List<B> shuffle<B>(List<B> list) = list;"
                + "def C chose<C>(List<C> list) = head(shuffle(list));");
    }

    @Test
    public void constructorTypeArgs() {
        assertNoTypeErrors("{ Maybe<Bool> o = Just(True); }");
    }

    @Test
    public void constructorTypeArgs2() {
        assertNoTypeErrors("data Foo<A> = Bar(A,A); { Foo<Bool> o = Bar(True,True); }");
    }

    @Test
    public void constructorTypeArgs3() {
        assertNoTypeErrors("data Foo<A,B> = Bar(A,B); { Foo<Bool,Int> o = Bar(True,5); }");
    }

    @Test
    public void constructorTypeArgs4() {
        assertNoTypeErrors("{ Either<Int,Bool> o = Left(5); }");
    }

    @Test
    public void testListArgs() {
        assertNoTypeErrors(" interface Database { } class DataBaseImpl(Map<String, List<String>> db) implements Database { } "
                + "{ Database db; db = new DataBaseImpl(map[Pair(\"file0\", list[\"file\", \"from\", \"db\"])]); }");

    }

    @Test
    public void testMaybeDataType() {
        assertNoTypeErrors("data MaybeTest<A> = NothingTest | JustTest(A);"
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
        assertNoTypeErrors("interface I { Bool m(); } class C(Bool b) implements I { Bool m() { return b; } }");

    }

    @Test
    public void newExp() {
        assertNoTypeErrors("class C(Bool b) { } { new C(True); }");
    }

}
