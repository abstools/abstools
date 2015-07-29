/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.parser;

import java.io.StringReader;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import abs.frontend.FrontendTest;
import abs.frontend.antlr.parser.ABSParserWrapper;
import abs.frontend.ast.CompilationUnit;
import abs.frontend.ast.DeltaDecl;

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
            "if True then 5 else 6"};

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
        assertParseOk(" "); // NO decls no block
    }

    @Test
    public void testEmptyBlock() {
        assertParseOk("{ }"); // No decls.
    }
    
    @Test
    public void ifExp() {
        assertParseOk("{ (if True then x else x).get; }" ); 
        assertParseOk("{ Int x = 5; if(if 4 == 5 then True else False) { x = 4; } else { x = 3; } }" ); 
        assertParseOk("{ if True then x else x.get; }" );
    }
    

    @Test
    public void testStmts() {
        assertParseOk("{ return x; }");
        assertParseOk("{   return x.get ;   }"); // one statement
        assertParseOk("{   skip ; return x.get ;    }"); // n statements
        assertParseOk("{ Int x ; Int y ;  skip ; return x.get ;   }"); // Variable
                                                                       // decls
        assertParseOk("{   skip  ; return x.get ;  }");
        assertParseOk("{ Fut<I> x ; J z ; }"); // need trailing semicolon here.
        assertParseOk(" { Fut<I> x ; Fut<Fut<I>> y ;  J z ; K w  ; }");
        assertParseOk("{ Int x = 5; Int y; Foo ref = null; List<Int> list = Cons(5, Nil); skip; }"); // Variable
                                                                                                     // decls
                                                                                                     // with/without
                                                                                                     // initializers
        //
    }

    // Interface declarations
    @Test
    public void testIfDecl() {
        assertParseOk(" interface Foo {} {}");
        assertParseOk(" interface Bar extends Bar1, Bar2 {} {}");
        assertParseError("interface Foo extends {} {}");
        assertParseError("interface extends {} {}");
    }

    // Class declarations
    @Test
    public void testClassDecl() {
        assertParseOk("class FooClass  {} {}");
        assertParseOk("class FooClass  implements Foo {} {}");
        assertParseOk("class FooClass  implements Foo {}"); // optional main
                                                            // body
        assertParseOk("class FooClass  implements Foo.Bar {}"); // qualified
                                                                // Name
        assertParseOk("class FooClass(T x , T y)  implements Foo {}"); // class
                                                                       // params
        assertParseOk("class FooClass(T x)  implements Foo {}"); // class params
        assertParseOk("class FooClass(Foo.T x)  implements Foo {}"); // class
                                                                     // params
                                                                     // qualified
                                                                     // name
        assertParseOk("class FooClass()  implements Foo {}"); // class params
        assertParseOk("class FooClass  implements Foo { T x ; }"); // field
        assertParseOk("class FooClass  implements Foo { Foo.T x ; }"); // field
                                                                       // qualified
        assertParseOk("class FooClass  implements Foo { T x ; { x = a ; }  }"); // init
                                                                                // block
        assertParseOk("class FooClass  implements Foo { T x = a ; }"); // field
                                                                       // with
                                                                       // initializer
        assertParseOk("class FooClass  implements Foo { {} } {} "); // empty
                                                                    // init
                                                                    // block
        assertParseOk(bbclass);
        assertParseError("class FooClass implements {}" + "{}");

    }

    // datatype declarations
    @Test
    public void testDatatypeDecl() {
        assertParseOk("data Foo = XCons | YCons ; ");
        assertParseOk("data IntList = IntNil | Cons(Int, IntList) ; ");
        assertParseOk("data IntList = IntNil | Cons(Prelude.Int, IntList) ; ");
    }
    
    @Test
    public void dataTypeSelectors() {
        assertParseOk("data Foo = Bla(Int i);");
        assertParseOk("data Foo = X | Bla(Int i);");
    }

    @Test
    public void testParametricDatatypeDecl() {
        assertParseOk("data List<A> = Nil | Cons(A, List<A>); ");
        assertParseOk("data Pair<A, B> = Pair(A, B); ");
    }

    @Test
    public void testFunctionDecl() {
        assertParseOk("def Int inc(Int x) = x;"); // fun_exp = IDENTIFIER
        assertParseOk("def Int inc(Int x) = plus(x,one());"); // fun_exp =
                                                              // Term(co,l)
        assertParseOk("def Datatype fn(Int x , Int y) = x;");
        assertParseOk("def TPair revPair(TPair p) = pair(snd(p),fst(p));");
        assertParseOk("def TPair revPair(TPair p) = Prelude.pair(Prelude.snd(p),fst(p));");
        // using let
        assertParseOk("def TPair revPair(TPair p) = let(T x) = fst(p) in pair(snd(p),x);");
        // using nested let
        assertParseOk("def TPair revPair(TPair p) = let(T x) = fst(p) in let(T y) = snd(p) in pair(y,x);");
    }

    @Test
    public void testParametricFunctionDecl() {
        assertParseOk("def Int length<A>(List<A> list) = case list { Nil => 0; Cons(_, rest) => 1 + length(rest); };");
        assertParseOk("def A nth<A>(List<A> list) = case n { 0 => head(list) ; _ => nth(tail(list), n-1) ; };");
    }

    @Test
    public void testAnnotations() {
        assertParseOk("[Test : \"value\"] class FooClass {} {}");
        assertParseOk("[Test : \"value\"] [Test: Nil] class FooClass {} {}");
        assertParseOk("[Test: 5] data Foo;");
        assertParseOk("[Test2 : Pair(5, \"value\")] data Pair<A, B> = Pair(A, B);");
        assertParseOk("[Test : 5] def Int constant() = 5;");
        assertParseOk("[Test: Nil] def A constant<A>(A a) = a;");
        assertParseOk("interface A { [Pre : x > 5] [Post : x > 0] Int method(Int x); }");
        assertParseOk("class A { [Method : Testable] Int method(Int x) { return x; } }");
        assertParseOk("class A { Int method(Int x) { [Value: Good] return x; } }");
        assertParseOk("[Block: Init]{ Int x = 1; [Stmt: \"conditional\"] if (x == 1) [Branch: Then] x = 5; else [Branch: Else] x = -1; }");
        assertParseOk("[Test] class FooClass {} {}");
        assertParseOk("[\"value\"] class FooClass {} {}");
        assertParseOk("class FooClass([Test] T t) {}");
        assertParseOk("class FooClass { [Test] T t; }");
        assertParseOk("class FooClass { Unit m([Test] T t) { }}");
        assertParseOk("class FooClass { Unit m() { [Test] T t; }}");
        assertParseOk("class FooClass { [Test] Unit m() { }}");
    }

    @Test
    public void testNAryConstructors() {
        assertParseOk("{ List<Int> l = list[]; }");
        assertParseOk("{ Set<String> s = set[\"one\", \"Two\", \"three\"]; }");
    }

    // comments
    @Test
    public void testComment() {
        assertParseOk("// one line\n");
        assertParseOk("/* Multi \n line \n comment */");
    }

    @Test
    public void testProg() {
        assertParseOk("interface Foo {} \n interface Bar extends Bar1, Bar2 {} \n class FooClass implements Foo {} {}");
    }

    @Test
    public void testAssignStmts() {

        for (String e : pureExp) {
            assertParseOk("{ v = " + e + ";}");
            assertParseOk("{ this.f = " + e + ";}");
        }
        for (String e : effExp) {
            assertParseOk("{ v = " + e + ";}");
            assertParseOk("{ this.f = " + e + ";}");
        }
        for (String e : eqExp) {
            assertParseOk("{ v = " + e + ";}");
            assertParseOk("{ this.f = " + e + ";}");
        }

        assertParseError("class A { A m() { this = null; return null;} }");

    }

    @Test
    public void testAwaitStmts() {
        String[] guards = { "y?", "y? & z? ", " y? & z? & w?",
        // any functional expression
        };
        for (String g : guards)
            assertParseOk("{ await " + g + " ; }");
    }

    @Test
    public void testOtherStmts() {
        String[] otherStmt = { "skip ; ", "suspend ; ", "return e ; " };
        for (String s : otherStmt)
            assertParseOk("{" + s + "}");
    }

    @Test
    public void testControlStmts() {

        assertParseOk("{ if (x) y = True  ; }");
        assertParseOk("{ if (x) y = True ; else y = False  ; }");
        assertParseOk("{ if (x) { y = True ; z = False; }  else y = False  ; }");
        assertParseOk("{ if (x) { if (y) skip ; else skip ; } else skip ; }");
        assertParseOk("{ if (x) if (y) skip ; else skip ; else skip ; }");
        assertParseOk("{ while (x) skip ;  }");
        assertParseOk("{ while (x) { x = y ; skip ; } }");
        assertParseOk("{ if (x) y = True ; else y = False  ; }");
        assertParseOk("{ if (x) { y = True ; z = False; }  else y = False  ; }");

    }

    @Test
    public void testStmtBlock() {
        assertParseOk("{ skip ; }");
        assertParseError(" { skip  }");

        assertParseOk("{ x = y ; skip ; }");
        assertParseError("{ x = y  skip  ; }");

        assertParseOk("{ x = y ; y = z ; skip  ; }");

        assertParseOk("{ { x = y ; skip ; await x? ; } skip ; }");
        assertParseOk("{ { x = y ; } skip ; }");
        assertParseOk(" { { } { } { } }  ");

    }

    @Test
    public void testStmtList() {
        assertParseOk("{ x = null; x = y.get ; x = y ; } ");

    }

    @Test
    public void moduleDeclQualName() {
        assertParseOk("module ABS.Lang;");
    }

    @Test
    public void importQual() {
        assertParseOk("import ABS.Test;");
    }

    @Test
    public void importFrom() {
        assertParseOk("import Test from ABS;");
    }

    @Test
    public void importFromList() {
        assertParseOk("import Test, Test2, fun from ABS;");
    }

    @Test
    public void importStarFrom() {
        assertParseOk("import * from ABS;");
    }

    @Test
    public void exportQual() {
        assertParseOk("export ABS.Test;");
    }

    @Test
    public void exportSimple() {
        assertParseOk("export Test;");
    }

    @Test
    public void exportList() {
        assertParseOk("export Test, fun;");
    }

    @Test
    public void exportStar() {
        assertParseOk("export *;");
    }
    
    @Test
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
        assertParseOk("module T238; delta D; productline P; features F,G; delta D when F && (! G);");
        // fails because logical connectives apart from && are not implemented yet...
    }
    
    @Test
    public void entry_deltadecl() throws Exception {
        CompilationUnit u = new ABSParserWrapper().parse(new StringReader("delta Mon;"));
        DeltaDecl d = (DeltaDecl) u.getDeltaDecl(0);
        Assert.assertNotNull(d);
    }
    
    @Test (expected = ParseException.class)
    public void deltaNameLowerCaseTest() throws Exception{
        String deltaDecl = "delta foo;";
        new ABSParserWrapper(null, true, false, false)
            .parse(new StringReader(deltaDecl));
    }
    
}
