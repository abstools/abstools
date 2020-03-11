/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.common;

import static org.junit.Assert.assertFalse;
import static org.junit.Assume.assumeFalse;

import java.io.File;

import org.abs_models.ABSTest;
import org.abs_models.backend.BackendTestDriver;
import org.abs_models.frontend.ast.Model;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;

@RunWith(Parameterized.class)
public class FunctionalTests extends SemanticTests {
    public FunctionalTests(BackendTestDriver d) {
        super(d);
    }

    static String CALL_F = "{ Bool testresult = f(); }";

    @Test
    public void funDef() throws Exception {
        assertEvalTrue("def Bool f() = True;" + CALL_F);
    }

    static String CALL_F_TRUE = "{ Bool testresult = f(True); }";

    @Test
    public void funArg() throws Exception {
        assertEvalTrue("def Bool f(Bool b) = b;" + CALL_F_TRUE);
    }

    @Test
    public void funParamteric() throws Exception {
        assertEvalTrue("def A f<A>(A a) = a;" + CALL_F_TRUE);
    }

    @Test
    public void dataType() throws Exception {
        assertEvalTrue("data Foo = Bar; { Bool testresult = True; Foo f = Bar(); }");
    }

    @Test
    public void dataTypeArgs() throws Exception {
        assertEvalTrue("data Foo = Bar(Bool); { Bool testresult = True; Foo f = Bar(True); }");
    }

    @Test
    public void dataTypeEq() throws Exception {
        assertEvalTrue("data Foo = Bar; { Bool testresult = Bar == Bar; }");
    }

    @Test
    public void dataTypeEq2() throws Exception {
        assertEvalTrue("data Foo = Bar(Bool); { Bool testresult = Bar(True) == Bar(True); }");
    }

    @Test
    public void dataTypeEq3() throws Exception {
        assertEvalTrue("data Foo = Bar(Bool); { Bool testresult = Bar(True) != Bar(False); }");
    }

    @Test
    public void dataTypeEq4() throws Exception {
        assertEvalTrue("{ Bool testresult = list[1, 2, 3] == list[1/1, 2/1, 3/1]; }");
    }
    
    @Test
    public void dataTypeEq5() throws Exception {
        // Test for a crash in the Erlang backend: rational numbers are
        // encoded as 2-tuples of integers, data types are encoded as tagged
        // tuples and were compared left-to-right.  This case reduced to a
        // division by zero.
        assertEvalTrue("data A = A(Int, Int); { Bool testresult = A(1, 0) == A(1, 0); }");
    }

    @Test
    public void dataTypeNEq() throws Exception {
        assertEvalTrue("{ Bool testresult = list[1, 2, 3] != list[1, 2]; }");
    }

    @Test
    public void dataTypeGt() throws Exception {
        assertEvalTrue("{ Bool testresult = Nil > Cons(10, Nil);  }");
    }

    @Test
    public void dataTypeGtNull() throws Exception {
        assertEvalTrue("interface I {} class C implements I {} { I i = new C(); Bool testresult = !(null > i);  }");
    }

    @Test
    public void dataTypeGt2() throws Exception {
        assertEvalTrue("{ Bool testresult = Cons(10, Nil) > Cons(5, Nil);  }");
    }

    @Test
    public void dataTypeGt3() throws Exception {
        assertEvalTrue("{ Bool testresult = Cons(10, Nil) > Cons(10, Nil) == False;  }");
    }

    @Test
    public void dataTypeLt() throws Exception {
        assertEvalTrue("{ Bool testresult = Cons(10, Nil) < Nil;  }");
    }

    @Test
    public void dataTypeLt2() throws Exception {
        assertEvalTrue("{ Bool testresult = Cons(5, Nil) < Cons(10, Nil);  }");
    }

    @Test
    public void dataTypeLt3() throws Exception {
        assertEvalTrue("{ Bool testresult = Cons(10, Nil) < Cons(10, Nil) == False;  }");
    }

    @Test
    public void dataTypeLtNull() throws Exception {
        assertEvalTrue("interface I {} class C implements I {} { I i = new C(); Bool testresult = null < i;  }");
    }

    @Test
    public void dataTypeLtEq() throws Exception {
        assertEvalTrue("{ Bool testresult = Cons(5, Nil) <= Cons(10, Nil);  }");
    }

    @Test
    public void dataTypeLtEqInner() throws Exception {
        assertEvalTrue("{ Bool testresult = Cons(5,Cons(3, Nil)) < Cons(5, Nil);  }");
    }

    @Test
    public void dataTypeLtEqNull() throws Exception {
        assertEvalTrue("interface I {} class C implements I {} { I i = new C(); Bool testresult = null <= i;  }");
    }

    @Test
    public void dataTypeLtEq2() throws Exception {
        assertEvalTrue("{ Bool testresult = Cons(5, Nil) <= Cons(5, Nil);  }");
    }

    @Test
    public void dataTypeGtEq() throws Exception {
        assertEvalTrue("{ Bool testresult = Cons(10, Nil) >= Cons(5, Nil);  }");
    }

    @Test
    public void dataTypeGtEqNull() throws Exception {
        assertEvalTrue("interface I {} class C implements I {} { I i = new C(); Bool testresult = !(null >= i);  }");
    }

    @Test
    public void dataTypeGtEq2() throws Exception {
        assertEvalTrue("{ Bool testresult = Cons(5, Nil) >= Cons(5, Nil);  }");
    }

    @Test
    public void dataTypeGtEqInner() throws Exception {
        assertEvalTrue("{ Bool testresult = Cons(5, Nil) > Cons(5,Cons(3, Nil));  }");
    }

    @Test
    public void parametericDataType() throws Exception {
        assertEvalTrue("data Foo<A> = Bar(A); { Bool testresult = True; Foo<Bool> f = Bar(True); }");
    }

    @Test
    public void testMaybeDataType() throws Exception {
        assertEvalTrue("data MaybeTest<A> = NothingTest | JustTest(A);"
                + "def B fromJustTest<B>(MaybeTest<B> a) = case a { JustTest(j) => j; }; "
                + "{ Bool testresult = fromJustTest(JustTest(True)); }");
    }

    @Test
    public void dataTypeSelector() throws Exception {
        assertEvalTrue("data Foo = Bar(Bool isTrue); " +
        		"{ Bool testresult = False; " +
        		"  Foo foo = Bar(True);  " +
        		"  testresult = isTrue(foo); }");
    }
    
    @Test
    public void exceptionDataType() throws Exception {
        assertEvalTrue("exception MyE(Bool); { Bool testresult = False; MyE e = MyE(True); switch (e) { MyE(value) => testresult = value; } }");
    }
    
    @Test
    public void exceptionSelector() throws Exception {
        assertEvalTrue("exception MyE(Bool myevalue); { MyE e = MyE(True); Bool testresult = myevalue(e); }");
    }
    
    
    @Test
    public void letExp() throws Exception {
        assertEvalTrue("def Bool f() = let (Bool x) = True in x;" + CALL_F);
    }

    @Test
    public void letExp2() throws Exception {
        assertEvalTrue("def Bool f() = let (Int i) = 5 in True;" + CALL_F);
    }

    @Test
    public void letExpNested() throws Exception {
        assertEvalTrue("def Bool f() = let (Bool x) = True in let (Bool y) = x in y;" + CALL_F);
    }

    @Test
    public void letExpNested2() throws Exception {
        assertEvalTrue("def Bool f() = let (Bool x) = True in let (Bool y) = False in x;" + CALL_F);
    }

    @Test
    public void letExpNested3() throws Exception {
        assertEvalTrue("def Bool f() = let (Bool x) = False in let (Bool x) = True in x;" + CALL_F);
    }

    @Test
    public void letExpMultiple() throws Exception {
        assertEvalTrue("def Bool f() = let Bool x = True, Bool y = x in y;" + CALL_F);
    }

    @Test
    public void letExpMultiple2() throws Exception {
        assertEvalTrue("def Bool f() = let Bool x = True, Bool y = False in x;" + CALL_F);
    }

    @Test
    public void letExpMultiple3() throws Exception {
        assertEvalTrue("def Bool f() = let Bool x = False, Bool x = True in x;" + CALL_F);
    }

    @Test
    public void letExpMultipleOld() throws Exception {
        assertEvalTrue("def Bool f() = let (Bool x) = True, (Bool y) = x in y;" + CALL_F);
    }

    @Test
    public void letExpMultipleOld2() throws Exception {
        assertEvalTrue("def Bool f() = let (Bool x) = True, (Bool y) = False in x;" + CALL_F);
    }

    @Test
    public void letExpMultipleOld3() throws Exception {
        assertEvalTrue("def Bool f() = let (Bool x) = False, (Bool x) = True in x;" + CALL_F);
    }

    @Test
    public void ifExp1() throws Exception {
        assertEvalTrue("def Bool f(Bool x) = when x then True else False ; " + CALL_F_TRUE);
    }

    @Test
    public void ifExp2() throws Exception {
        assertEvalTrue("def Bool f(Bool x) = when !x then False else True ; " + CALL_F_TRUE);
    }

    @Test
    public void caseTrue() throws Exception {
        assertEvalTrue("def Bool f(Bool x) = case x { True => True; False => False; }; " + CALL_F_TRUE);
    }

    @Test
    public void casePatternVar() throws Exception {
        assertEvalTrue("data Foo = Bar(Bool); def Bool f(Foo x) = case x { Bar(y) => y; }; { Bool testresult = f(Bar(True)); }");
    }

    @Test
    public void casePatternUnderscore() throws Exception {
        assertEvalTrue("def Bool f(Bool x) = case x { _ => True; };" + CALL_F_TRUE);
    }

    @Test
    public void casePatternBoundVar() throws Exception {
        assertEvalTrue("def Bool f(Bool x) = let (Bool y) = True in case x { y => True; };" + CALL_F_TRUE);
    }

    @Test
    public void casePatternBoundVar2() throws Exception {
        assertEvalTrue("def Bool f(Bool x) = let (Bool y) = False in case x { y => False; z => True; };" + CALL_F_TRUE);
    }

    @Test
    public void casePatternStringLiteral() throws Exception {
        assertEvalTrue("def Bool f() = let (String s) = \"foo\" in case s { \"bar\" => False; \"foo\" => True; };"
                + CALL_F);
    }

    @Test
    public void casePatternIntLiteral() throws Exception {
        assertEvalTrue("def Bool f() = let (Int i) = 4 in case i { 2 => False; 4 => True; };" + CALL_F);
    }
    
    @Test
    public void caseField() throws Exception {
        assertEvalTrue(new File("abssamples/backend/FunctionalTests/caseField.abs"));
     }

    @Test
    public void caseBoundLocalField() throws Exception {
        assertEvalTrue(new File("abssamples/backend/FunctionalTests/caseBoundLocal.abs"));
     }

    @Test
    public void caseBoundLet() throws Exception {
        assertEvalTrue(new File("abssamples/backend/FunctionalTests/caseBoundLet.abs"));
     }

    @Test
    public void caseBoundParameter() throws Exception {
        assertEvalTrue(new File("abssamples/backend/FunctionalTests/caseBoundParameter.abs"));
     }

    @Test
    public void caseBoundClassParameter() throws Exception {
        assertEvalTrue(new File("abssamples/backend/FunctionalTests/caseBoundClassParameter.abs"));
     }

    @Test
    public void caseBoundAssignedParameter() throws Exception {
        assertEvalTrue(new File("abssamples/backend/FunctionalTests/caseBoundAssignedParameter.abs"));
     }

    @Test
    public void typeSynonyms() throws Exception {
        assertEvalTrue("type Data = Int; { Int i = 5; Data d = 5; Bool testresult = d == i; }");
    }

    @Test
    public void assertStmt() throws Exception {
        assertEvalTrue("{ assert True; Bool testresult = True; }");
    }

    @Test
    public void assertStmtFails() throws Exception {
        assertEvalFails("{ assert False; Bool testresult = True; }");
    }

    @Test
    public void setLength() throws Exception {
        assertEvalTrue("{ Set<Int> s = set[4,4,4]; Bool testresult = (size(s) == 1);}");
    }

    /* https://github.com/abstools/abstools/issues/62 - Int vs. Rat */
    @Test
    public void testPow1() throws Exception {
        assertEvalTrue("def Rat pow2(Int n, Int i) = when i < 0 then 1 / pow2(n, -i) else case i { 0 => 1; _ => n * pow2(n, i-1);  }; { Bool testresult = True; }");
    }

    @Test
    public void testPow2() throws Exception {
        assertEvalTrue("def Rat pow2(Int n, Int i) = when i < 0 then 1 / pow2(n, -i) else case i { 0 => 1; _ => n * pow2(n, i-1);  }; { Bool testresult = True; }");
    }
    
    @Test
    public void testIntRatCase() throws Exception {
        assertEvalTrue("def Rat pow2(Int n, Int i) = case i < 0 { True => 1 / pow2(n, -i); False => case i { 0 => 1; _ => n * pow2(n, i-1);};  }; { Bool testresult = True; }");
    }

    @Test
    public void patternVarRew() throws Exception {
        String fileName = "abssamples/PVTest.abs";
        Model m = ABSTest.assertParseFileOk(fileName);
        // TODO: Pull up
        // XXX WTF?! assertFalse(m.hasParserErrors());
        m.flattenForProduct("Foo");
        assertFalse(m.hasTypeErrors());
        assertEvalTrue(m);
    }
    
    @Test
    public void patternVarRewOK() throws Exception {
        assertEvalTrue(new File("abssamples/PVTest2.abs"));
    }

    @Test
    public void wrappedSupertype() throws Exception {
        // https://github.com/abstools/abstools/issues/241
        assumeFalse(driver.getBackendName() == BackendTestDriver.BackendName.JAVA);
        assertEvalTrue(new File("abssamples/backend/FunctionalTests/wrappedSupertype.abs"));
    }

    @Test
    public void templatestrings() throws Exception {
        assertEvalTrue(new File("abssamples/backend/FunctionalTests/templatestrings.abs"));
    }
}
