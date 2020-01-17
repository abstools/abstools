/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.frontend.parser;

import java.io.StringReader;

import org.abs_models.frontend.FrontendTest;
import org.abs_models.frontend.ast.CompilationUnit;
import org.abs_models.frontend.ast.DeltaDecl;
import org.abs_models.frontend.ast.Model;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import static org.junit.Assert.fail;
import static org.junit.Assert.assertTrue;

public class ParserTest extends FrontendTest {

    // private String emptyblock ;
    private String bbclass;
    // private String ms1, ms2, meth1, meth2 , fields, comment, comment2 ;

    private String[] pureExp = { " x ", " this.x ",
            "null",
            // function expressions
            // data constructor expression
            "Int(x , y)",
            // function application
            "pluss(x,y)", "nth(tail(file,n))",
            // pair constructor
            "pair(x,y)",
            // case expression
            "case set { EmptyStringSet => False ; }", "case set { EmptyStringSet() => False ; }",
            "case set { EmptyStringSet => False ; InsertString(string2,set2) => True ;}",
            "when True then 5 else 6"};

    private String[] effExp = { "new local Foo()  ", "new local Foo(a,b)  ", "o!init()  ", "o!init(y)  ", "o!init(y,z)  ",
            "this!init(y,z)  ", "o.init(y,z,w)  ",
            // "this.init(y,z)",
            "y.get" };

    private String[] eqExp = { "a == b " };

    @Before
    public void setUp() {

        // emptyblock = "{    }";
        // methodsignatures
        // ms1 = "Unit init(Foo x ,  Bar y)";
        // ms2 = "Unit append(Int i)";
        // meth1 = ms1 + "{ Int x ;  Int y ;	return null ; }";
        // meth2 = ms2 + "{ skip; return null ; }";
        bbclass = "class BoundedBuffer implements Buffer { \n" + "  ListofInt buffer ;     Int max ;      Int n ;	\n"
                + "  Unit init(Foo x){ Int x ;  Int y ;  return null ; }\n"
                + "  Unit append(Int i){ skip; return null ; }}";
    }

    @Test
    public void testNothing() {
        // NO decls no block
        assertParse(" ");
    }

    @Test
    public void testEmptyBlock() {
        // No decls.
        Model m = assertParse("{ }");
        assertTrue(m.lookupModule("UnitTest").hasBlock());
    }

    @Test
    public void ifExp() {
        assertParse("{ (when True then x else x).get; }");
        assertParse("{ Int x = 5; if(when 4 == 5 then True else False) { x = 4; } else { x = 3; } }");
    }


    @Test
    public void testStmts() {
        assertParse("{ return x; }");
        // one statement
        assertParse("{   return x.get ;   }");
        // n statements
        assertParse("{   skip ; return x.get ;    }");
        // Variable
        assertParse("{ Int x ; Int y ;  skip ; return x.get ;   }");
        // decls
        assertParse("{   skip  ; return x.get ;  }");
        // need trailing semicolon here.
        assertParse("{ Fut<I> x ; J z ; }");
        assertParse(" { Fut<I> x ; Fut<Fut<I>> y ;  J z ; K w  ; }");
        // Variable
        assertParse("{ Int x = 5; Int y; Foo ref = null; List<Int> list = Cons(5, Nil); skip; }");
        // decls
                                                                                                     // with/without
                                                                                                     // initializers
        //
    }

    // Interface declarations
    @Test
    public void testIfDecl() {
        assertParse(" interface Foo {} {}");
        assertParse(" interface Bar extends Bar1, Bar2 {} {}");
        assertParseError("interface Foo extends {} {}");
        assertParseError("interface extends {} {}");
    }

    // Class declarations
    @Test
    public void testClassDecl() {
        assertParse("class FooClass  {} {}");
        // optional main body
        assertParse("class FooClass  implements Foo {} {}");
        assertParse("class FooClass  implements Foo {}");
        // qualified Name
        assertParse("class FooClass  implements Foo.Bar {}");
        // class params
        assertParse("class FooClass(T x , T y)  implements Foo {}");
        // class params
        assertParse("class FooClass(T x)  implements Foo {}");
        // class params qualified name
        assertParse("class FooClass(Foo.T x)  implements Foo {}");
        // class params
        assertParse("class FooClass()  implements Foo {}");
        // field
        assertParse("class FooClass  implements Foo { T x ; }");
        // field qualified
        assertParse("class FooClass  implements Foo { Foo.T x ; }");
        // init block
        assertParse("class FooClass  implements Foo { T x ; { x = a ; }  }");
        // field with initializer
        assertParse("class FooClass  implements Foo { T x = a ; }");
        // empty init block
        assertParse("class FooClass  implements Foo { {} } {} ");
        assertParse(bbclass);
        assertParseError("class FooClass implements {}" + "{}");
    }

    @Test
    public void testTraitDecl() {
        // trait with annotation
        assertParse("[Cost: 15] trait T = { }");
        assertParse("trait T = Unit foo() { }");
        assertParse("trait T = { Unit foo() { } Unit bar() { } }");
        assertParse("trait T = Unit foo() { } removes Unit foo(); adds Unit foo() { }");
        assertParse("trait T = { Unit foo() { } } modifies Unit foo() { }");
        assertParse("trait T = { Unit foo() { } Unit bar() { } } removes { Unit foo(); Unit bar(); } ");
    }

    // datatype declarations
    @Test
    public void testDatatypeDecl() {
        assertParse("data Foo = XCons | YCons ; ");
        assertParse("data IntList = IntNil | Cons(Int, IntList) ; ");
        assertParse("data IntList = IntNil | Cons(Prelude.Int, IntList) ; ");
    }

    @Test
    public void dataTypeSelectors() {
        assertParse("data Foo = Bla(Int i);");
        assertParse("data Foo = X | Bla(Int i);");
    }

    @Test
    public void testParametricDatatypeDecl() {
        assertParse("data List<A> = Nil | Cons(A, List<A>); ");
        assertParse("data Pair<A, B> = Pair(A, B); ");
    }

    @Test
    public void testFunctionDecl() {
        // fun_exp = IDENTIFIER
        assertParse("def Int inc(Int x) = x;");
        // fun_exp = Term(co,l)
        assertParse("def Int inc(Int x) = plus(x,one());");
        assertParse("def Datatype fn(Int x , Int y) = x;");
        assertParse("def TPair revPair(TPair p) = pair(snd(p),fst(p));");
        assertParse("def TPair revPair(TPair p) = Prelude.pair(Prelude.snd(p),fst(p));");
        // using let
        assertParse("def TPair revPair(TPair p) = let(T x) = fst(p) in pair(snd(p),x);");
        // using nested let
        assertParse("def TPair revPair(TPair p) = let(T x) = fst(p) in let(T y) = snd(p) in pair(y,x);");
    }

    @Test
    public void testBuiltinFunctionDecl() {
        assertParse("def Int f() = builtin;");
        assertParse("def Int f() = builtin[];");
        assertParse("def Int f() = builtin[\"a\"];");
        assertParse("def Int f() = builtin[\"a\", \"b\", \"c\"];");
    }

    @Test
    public void testParametricFunctionDecl() {
        assertParse("def Int length<A>(List<A> list) = case list { Nil => 0; Cons(_, rest) => 1 + length(rest); };");
        assertParse("def A nth<A>(List<A> list) = case n { 0 => head(list) ; _ => nth(tail(list), n-1) ; };");
    }

    @Test
    public void testAnnotations() {
        assertParse("[Test : \"value\"] class FooClass {} {}");
        assertParse("[Test : \"value\"] [Test: Nil] class FooClass {} {}");
        assertParse("[Test : \"value\", Test: Nil] class FooClass {} {}");
        assertParse("[Test: 5] data Foo;");
        assertParse("[Test2 : Pair(5, \"value\")] data Pair<A, B> = Pair(A, B);");
        assertParse("[Test : 5] def Int constant() = 5;");
        assertParse("[Test: Nil] def A constant<A>(A a) = a;");
        assertParse("interface A { [Pre : x > 5] [Post : x > 0] Int method(Int x); }");
        assertParse("interface A { [Pre : x > 5, Post : x > 0] Int method(Int x); }");
        assertParse("class A { [Method : Testable] Int method(Int x) { return x; } }");
        assertParse("class A { Int method(Int x) { [Value: Good] return x; } }");
        assertParse("[Block: Init]{ Int x = 1; [Stmt: \"conditional\"] if (x == 1) [Branch: Then] x = 5; else [Branch: Else] x = -1; }");
        assertParse("[Test] class FooClass {} {}");
        assertParse("[\"value\"] class FooClass {} {}");
        assertParse("class FooClass([Test] T t) {}");
        assertParse("class FooClass { [Test] T t; }");
        assertParse("class FooClass { Unit m([Test] T t) { }}");
        assertParse("class FooClass { Unit m() { [Test] T t; }}");
        assertParse("class FooClass { [Test] Unit m() { }}");
    }

    @Test
    public void testNAryConstructors() {
        assertParse("{ List<Int> l = list[]; }");
        assertParse("{ Set<String> s = set[\"one\", \"Two\", \"three\"]; }");
    }

    // comments
    @Test
    public void testComment() {
        assertParse("// one line\n");
        assertParse("/* Multi \n line \n comment */");
    }

    @Test
    public void testProg() {
        assertParse("interface Foo {} \n interface Bar extends Bar1, Bar2 {} \n class FooClass implements Foo {} {}");
    }

    @Test
    public void testAssignStmts() {

        for (String e : pureExp) {
            assertParse("{ v = " + e + ";}");
            assertParse("{ this.f = " + e + ";}");
        }
        for (String e : effExp) {
            assertParse("{ v = " + e + ";}");
            assertParse("{ this.f = " + e + ";}");
        }
        for (String e : eqExp) {
            assertParse("{ v = " + e + ";}");
            assertParse("{ this.f = " + e + ";}");
        }

        assertParseError("class A { A m() { this = null; return null;} }");

    }

    @Test
    public void testAwaitStmts() {
        String[] guards = { "y?", "y? & z? ", " y? & z? & w?",
        // any functional expression
        };
        for (String g : guards)
            assertParse("{ await " + g + " ; }");
    }

    @Test
    public void testOtherStmts() {
        String[] otherStmt = { "skip ; ", "suspend ; ", "return e ; " };
        for (String s : otherStmt)
            assertParse("{" + s + "}");
    }

    @Test
    public void testControlStmts() {

        assertParse("{ if (x) y = True  ; }");
        assertParse("{ if (x) y = True ; else y = False  ; }");
        assertParse("{ if (x) { y = True ; z = False; }  else y = False  ; }");
        assertParse("{ if (x) { if (y) skip ; else skip ; } else skip ; }");
        assertParse("{ if (x) if (y) skip ; else skip ; else skip ; }");
        assertParse("{ while (x) skip ;  }");
        assertParse("{ while (x) { x = y ; skip ; } }");
        assertParse("{ if (x) y = True ; else y = False  ; }");
        assertParse("{ if (x) { y = True ; z = False; }  else y = False  ; }");

    }

    @Test
    public void testStmtBlock() {
        assertParse("{ skip ; }");
        assertParseError(" { skip  }");

        assertParse("{ x = y ; skip ; }");
        assertParseError("{ x = y  skip  ; }");

        assertParse("{ x = y ; y = z ; skip  ; }");

        assertParse("{ { x = y ; skip ; await x? ; } skip ; }");
        assertParse("{ { x = y ; } skip ; }");
        assertParse(" { { } { } { } }  ");

    }

    @Test
    public void testStmtList() {
        assertParse("{ x = null; x = y.get ; x = y ; } ");

    }

    @Test
    public void moduleDeclQualName() {
        assertParse("module ABS.Lang;");
    }

    @Test
    public void importQual() {
        assertParse("import ABS.Test;");
    }

    @Test
    public void importFrom() {
        assertParse("import Test from ABS;");
    }

    @Test
    public void importFromList() {
        assertParse("import Test, Test2, fun from ABS;");
    }

    @Test
    public void importStarFrom() {
        assertParse("import * from ABS;");
    }

    @Test
    public void exportQual() {
        assertParse("export ABS.Test;");
    }

    @Test
    public void exportSimple() {
        assertParse("export Test;");
    }

    @Test
    public void exportList() {
        assertParse("export Test, fun;");
    }

    @Test
    public void exportStar() {
        assertParse("export *;");
    }

    @Test @Ignore("xtext branch bug #266")
    public void ticket189() {
        assertParseError("def Unit foo() = Unit;\n"+
         "class Bob { \n" +
         "   Unit run() { " +
         "      case Nil {"+
               "    Nil => Unit;"+
               "   _ => foo() // Note missing semicolon\n"+
               "}; \n" +
             "} \n" +
          "} \n");
    }


    @Test
    public void ticket203() {
        assertParseError("def Bool g() = f(s a, s b);");
    }

    @Test
    public void ticket238() throws Exception{
        assertParse("module T238; delta D; productline P; features F,G; delta D when F && (! G);");
        // fails because logical connectives apart from && are not implemented yet...
    }

    @Test
    public void ticket267() {
        assertParseError("module Test; data D; import * from ABS.StdLib;");
    }

    @Test
    public void entry_deltadecl() throws Exception {
        CompilationUnit u = parseString("delta Mon;").getCompilationUnit(1);
        DeltaDecl d = (DeltaDecl) u.getDeltaDecl(0);
        Assert.assertNotNull(d);
    }

    public void deltaNameLowerCaseTest() throws Exception{
        String deltaDecl = "delta foo;";
        assertParseError(deltaDecl);
    }

    public void testIllegalCharacter() throws Exception {
        String functionDecl = "module LexicalTest; def Bool æåëßfë() = True;";
        assertParseError(functionDecl);
    }

    @Test
    public void partialFunctionDecl() {
        // fun_exp = IDENTIFIER
        assertParse("def Int inc()(Int x) = x;");
        assertParse("def Int inc(f_1)(Int x) = x;");
        assertParse("def Int inc(f_1, f_2)(Int x) = x;");
        assertParse("def Datatype fn()(Int x , Int y) = x;");

        // fun_exp = Term(co,l)
        assertParse("def Int inc()(Int x) = plus(x,one());");

        // fun_exp = f_1(x)
        assertParse("def Int apply(f_1)(Int x) = f_1(x);");

        // fun_exp = Term(f_1(x))
        assertParse("def Int applyAndInc(f_1)(Int x) = plus(f_1(x), one());");

        assertParse("def TPair revPair(f_1)(TPair p) = pair(snd(f_1(p)),fst(p));");
        // using let
        assertParse("def TPair revPair()(TPair p) = let(T x) = fst(p) in pair(snd(p),x);");
        assertParse("def TPair revPair(f_1)(TPair p) = let(T x) = f_1(fst(p)) in pair(snd(p),x);");
        assertParse("def TPair revPair(f_1)(TPair p) = let(T x) = fst(p) in pair(f_1(snd(p)),x);");
        // using nested let
        assertParse("def TPair revPair()(TPair p) = let(T x) = fst(p) in let(T y) = snd(p) in pair(y,x);");
        assertParse("def TPair revPair(f_1)(TPair p) = let(T x) = fst(p) in let(T y) = f_1(snd(p)) in pair(y,x);");
        assertParse("def TPair revPair(f_1)(TPair p) = let(T x) = fst(p) in let(T y) = snd(p) in pair(f_1(y),x);");
    }

    @Test
    public void parametricPartialFunctionDecl() {
        assertParse("def Int length<A>(f_1)(List<A> list) = case list { Nil => 0; Cons(_, rest) => 1 + length(rest); };");
        assertParse("def Int length<A>(inc, dec)(List<A> list) = case list { Nil => 0; Cons(_, rest) => inc(dec(1)) + length(rest); };");
        assertParse("def A nth<A>(f_1)(List<A> list) = case n { 0 => head(list) ; _ => nth(tail(list), n-1) ; };");
    }

    @Test
    public void partialFunctionTypedFunctionParameter() {
        assertParseError("def Int add(Int x)(Int y) = y;");
        assertParseError("def Int add(f_1, Int x)(Int y) f_1(y);");
    }

    @Test
    public void partialFunctionInvalidParameterSymbols() {
        assertParseError("def Int identity(f1<Int>)(Int y) = y;");
        assertParseError("def Int zero(f-n)() = 0;");
        assertParseError("def Int zero(f!n)() = 0;");
        assertParseError("def Int zero(f?n)() = 0;");
    }

    @Test
    public void callPartialFunction() {
        assertParse("{ f(g)(); }");
        assertParse("{ f(g, h)(); }");
        assertParse("{ f(some_func, other_func)(); }");
    }

    @Test
    public void callPartialFunctionInvalidParams() {
        assertParseError("{ f(g(x))(); }");
        assertParseError(" { f(f g)(); }");
        assertParseError(" { f(f())(); }");
        assertParseError(" { f(g())(); }");
        assertParseError(" { f(0)(); }");
    }

    @Test
    public void anonymousFunction() {
        assertParse("{ f((Int i) => i)(); }");
        assertParse("{ f((Int i) => i + 1)(); }");
        assertParse("{ f((Int i) => inc(i))(); }");
        assertParse("{ f((Int i) => 0)(); }");
        assertParse("{ f(() => 0)(); }");
        assertParse("{ f((Int i) => i)(); }");
        assertParse("{ f((Int i, Int j) => i + j)(); }");
    }

    @Test
    public void multipleAnonymousFunctions() {
        assertParse("{ f((Int i) => i, (Int j) => j)(); }");
        assertParse("{ f((Int i) => i, inc)(); }");
    }

    @Test
    public void anonymousFunctionNoParamBraces() {
        assertParseError("{ f(Int i => i)();}");
    }

    /**
     * Before commit b79ac958b150bda90acb3c095bba0c30d97df5e4 this caused an
     * error, since list literals were not considered when determining the
     * free variables within an anonymous function (closure).
     */
    @Test
    public void anonymousFunctionFreeVarsInList() throws Exception {
        final String fileName = "abssamples/ClosureWithListLiterals.abs";
        final Model m = assertParseFileOk(fileName);

        if (m.hasParserErrors())
            fail(m.getParserErrors().get(0).toString());
    }
}
