/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java;

import org.junit.Test;

public class JavaExprTests extends JavaBackendTest {

    @Test
    public void ifExp() throws Exception {
        assertValidStdLib("def Bool f(Bool x) = if x then True else False; ");
    }

    @Test
    public void ifInLetExp() throws Exception {
        // see bug 343
        assertValidStdLib("def Int f(Int x) = let (Int y) = 3 in if x == y then 42 else 41; ");
    }
    
    @Test
    public void caseTrue() throws Exception {
        assertValidStdLib("def Bool f(Bool x) = case x { True => True; False => False; }; ");
    }

    @Test
    public void casePatternVar() throws Exception {
        assertValidStdLib("data Foo = Bar(Bool); def Bool f(Foo x) = case x { Bar(y) => y; }; ");
    }

    @Test
    public void appendright() throws Exception {
        assertValidStdLib("def List<A> appendrightTest<A>(List<A> list, A p) = concatenate(list, Cons(p, Nil));");
    }

    @Test
    public void ticket253() throws Exception {
        assertValidStdLib("data F<A> = D(String) | E(A) ; def F<A> f<A,B>(F<B> d) = case d { D(x) => D(x); };");
    }
    
    @Test
    public void getExpr() throws Exception {
        assertValidStdLib("{ Fut<String> fu; fu.get; }");
    }

    @Test
    public void useofJavaKeywordsVariable() throws Exception {
        assertValidStdLib("{ Bool continue = False; }");
    }

    @Test
    public void useofJavaKeywordsField() throws Exception {
        assertValidStdLib("class C(Bool continue) { Unit m() { Bool goto = continue; }}");
    }

    @Test
    public void useofJavaKeywordsMethod() throws Exception {
        assertValidStdLib("class C() { Unit continue() { this.continue(); }}");
    }

    @Test
    public void useofJavaKeywordsMethodInterface() throws Exception {
        assertValidStdLib("interface I { Unit continue(); } { I i; i.continue(); }");
    }

    @Test
    public void useofJavaKeywordsFunction() throws Exception {
        assertValidStdLib("def Unit continue() = Unit; { Unit u = continue(); }");
    }

    @Test
    public void useofJavaKeywordsPattern() throws Exception {
        assertValidStdLib("def Bool continue(Bool break) = case break { false => break; };");
    }

    @Test
    public void useOfVariablesInsideCase() throws Exception {
        assertValidStdLib("{ Bool b = True; Bool c = case b { _ => b; }; }");
    }

    @Test
    public void useOfVariablesInsideLet() throws Exception {
        assertValidStdLib("{ Bool b = True; Bool c = let (Bool x) = True in b; }");
    }

    @Test
    public void test_thisDC_ticket318() throws Exception {
        assertValidJava(getJavaCode("module M; import * from ABS.DC; { DeploymentComponent dc = thisDC(); }", Config.WITH_STD_LIB));
    }
    
    
    @Test
    public void caseExpr1() throws Exception {
        assertValidJavaExecution(true,
                "module Test;",
                "data MyData = MyDataCons | MyNothing;",
                "{",
                    "assert case MyDataCons {",
                    "   Test.MyDataCons => True;",
                    "   MyDataCons => False;",
                    "};",
                "}"
        );
    }
    
    @Test
    public void caseExpr2() throws Exception {
        assertValidJavaExecution(true,
                "module Test;",
                "data MyData = MyDataCons | MyNothing;",
                "{",
                    "assert case MyDataCons {",
                    "   MyDataCons => True;",
                    "   Test.MyDataCons => False;",
                    "};",
                "}"
        );
    }
    
}
