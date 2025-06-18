/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java;

import org.junit.Test;

public class JavaExprTests extends JavaBackendTest {

    @Test
    public void ifExp() throws Exception {
        assertValid("def Bool f(Bool x) = when x then True else False; ");
    }

    @Test
    public void ifInLetExp() throws Exception {
        // see bug 343
        assertValid("def Int f(Int x) = let (Int y) = 3 in when x == y then 42 else 41; ");
    }
    
    @Test
    public void caseTrue() throws Exception {
        assertValid("def Bool f(Bool x) = case x { True => True; False => False; }; ");
    }

    @Test
    public void casePatternVar() throws Exception {
        assertValid("data Foo = Bar(Bool); def Bool f(Foo x) = case x { Bar(y) => y; }; ");
    }

    @Test
    public void appendright() throws Exception {
        assertValid("def List<A> appendrightTest<A>(List<A> list, A p) = concatenate(list, Cons(p, Nil));");
    }

    @Test
    public void ticket253() throws Exception {
        assertValid("data F<A> = D(String) | E(A) ; def F<A> f<A,B>(F<B> d) = case d { D(x) => D(x); };");
    }
    
    @Test
    public void getExpr() throws Exception {
        assertValid("{ Fut<String> fu; fu.get; }");
    }

    @Test
    public void useofJavaKeywordsVariable() throws Exception {
        assertValid("{ Bool continue = False; }");
    }

    @Test
    public void useofJavaKeywordsField() throws Exception {
        assertValid("class C(Bool continue) { Unit m() { Bool goto = continue; }}");
    }

    @Test
    public void useofJavaKeywordsMethod() throws Exception {
        assertValid("class C() { Unit continue() { this.continue(); }}");
    }

    @Test
    public void useofJavaKeywordsMethodInterface() throws Exception {
        assertValid("interface I { Unit continue(); } { I i; i.continue(); }");
    }

    @Test
    public void useofJavaKeywordsFunction() throws Exception {
        assertValid("def Unit continue() = Unit; { Unit u = continue(); }");
    }

    @Test
    public void useofJavaKeywordsPattern() throws Exception {
        assertValid("def Bool continue(Bool break) = case break { False => break; };");
    }

    @Test
    public void useOfVariablesInsideCase() throws Exception {
        assertValid("{ Bool b = True; Bool c = case b { _ => b }; }");
    }

    @Test
    public void useOfVariablesInsideLet() throws Exception {
        assertValid("{ Bool b = True; Bool c = let (Bool x) = True in b; }");
    }

    @Test
    public void test_thisDC_ticket318() throws Exception {
        assertValidJava(getJavaCode("module M; import * from ABS.DC; { DeploymentComponent dc = thisDC(); }"));
    }

}
