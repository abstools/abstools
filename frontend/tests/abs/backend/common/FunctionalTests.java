/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.common;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;

import abs.backend.BackendTestDriver;

@RunWith(Parameterized.class)
public class FunctionalTests extends SemanticTests {
    public FunctionalTests(BackendTestDriver d) {
        super(d);
    }

    static String CALL_F = "{ Bool testresult = f(); }";

    @Test
    public void funDef() {
        assertEvalTrue("def Bool f() = True;" + CALL_F);
    }

    static String CALL_F_TRUE = "{ Bool testresult = f(True); }";

    @Test
    public void funArg() {
        assertEvalTrue("def Bool f(Bool b) = b;" + CALL_F_TRUE);
    }

    @Test
    public void funParamteric() {
        assertEvalTrue("def A f<A>(A a) = a;" + CALL_F_TRUE);
    }

    @Test
    public void dataType() {
        assertEvalTrue("data Foo = Bar; { Bool testresult = True; Foo f = Bar(); }");
    }

    @Test
    public void dataTypeArgs() {
        assertEvalTrue("data Foo = Bar(Bool); { Bool testresult = True; Foo f = Bar(True); }");
    }

    @Test
    public void dataTypeEq() {
        assertEvalTrue("data Foo = Bar; { Bool testresult = Bar == Bar; }");
    }

    @Test
    public void dataTypeEq2() {
        assertEvalTrue("data Foo = Bar(Bool); { Bool testresult = Bar(True) == Bar(True); }");
    }

    @Test
    public void dataTypeEq3() {
        assertEvalTrue("data Foo = Bar(Bool); { Bool testresult = Bar(True) != Bar(False); }");
    }

    @Test
    public void parametericDataType() {
        assertEvalTrue("data Foo<A> = Bar(A); { Bool testresult = True; Foo<Bool> f = Bar(True); }");
    }

    @Test
    public void testMaybeDataType() {
        assertEvalTrue("data MaybeTest<A> = NothingTest | JustTest(A);"
                + "def B fromJustTest<B>(MaybeTest<B> a) = case a { JustTest(j) => j; }; "
                + "{ Bool testresult = fromJustTest(JustTest(True)); }");
    }

    @Test
    public void dataTypeSelector() {
        assertEvalTrue("data Foo = Bar(Bool isTrue); " +
        		"{ Bool testresult = False; " +
        		"  Foo foo = Bar(True);  " +
        		"  testresult = isTrue(foo); }");
    }
    
    @Test
    public void letExp() {
        assertEvalTrue("def Bool f() = let (Bool x) = True in x;" + CALL_F);
    }

    @Test
    public void letExp2() {
        assertEvalTrue("def Bool f() = let (Int i) = 5 in True;" + CALL_F);
    }

    @Test
    public void letExpNested() {
        assertEvalTrue("def Bool f() = let (Bool x) = True in let (Bool y) = x in y;" + CALL_F);
    }

    @Test
    public void letExpNested2() {
        assertEvalTrue("def Bool f() = let (Bool x) = True in let (Bool y) = False in x;" + CALL_F);
    }

    @Test
    public void letExpNested3() {
        assertEvalTrue("def Bool f() = let (Bool x) = False in let (Bool x) = True in x;" + CALL_F);
    }

    @Test
    public void ifExp() {
        assertEvalTrue("def Bool f(Bool x) = if x then True else False ; " + CALL_F_TRUE);
        assertEvalTrue("def Bool f(Bool x) = if ~x then False else True ; " + CALL_F_TRUE);
    }

    @Test
    public void caseTrue() {
        assertEvalTrue("def Bool f(Bool x) = case x { True => True; False => False; }; " + CALL_F_TRUE);
    }

    @Test
    public void casePatternVar() {
        assertEvalTrue("data Foo = Bar(Bool); def Bool f(Foo x) = case x { Bar(y) => y; }; { Bool testresult = f(Bar(True)); }");
    }

    @Test
    public void casePatternUnderscore() {
        assertEvalTrue("def Bool f(Bool x) = case x { _ => True; };" + CALL_F_TRUE);
    }

    @Test
    public void casePatternBoundVar() {
        assertEvalTrue("def Bool f(Bool x) = let (Bool y) = True in case x { y => True; };" + CALL_F_TRUE);
    }

    @Test
    public void casePatternBoundVar2() {
        assertEvalTrue("def Bool f(Bool x) = let (Bool y) = False in case x { y => False; z => True; };" + CALL_F_TRUE);
    }

    @Test
    public void casePatternStringLiteral() {
        assertEvalTrue("def Bool f() = let (String s) = \"foo\" in case s { \"bar\" => False; \"foo\" => True; };"
                + CALL_F);
    }

    @Test
    public void casePatternIntLiteral() {
        assertEvalTrue("def Bool f() = let (Int i) = 4 in case i { 2 => False; 4 => True; };" + CALL_F);
    }

    @Test
    public void typeSynonyms() {
        assertEvalTrue("type Data = Int; { Int i = 5; Data d = 5; Bool testresult = d == i; }");
    }

    @Test
    public void assertStmt() {
        assertEvalTrue("{ assert True; Bool testresult = True; }");
    }

    @Test
    public void assertStmtFails() throws Exception {
        assertEvalFails("{ assert False; Bool testresult = True; }");
    }
}
