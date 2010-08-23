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
    public void caseTrue() {
        assertEvalTrue("def Bool f(Bool x) = case x { True => True; False => False; }; "+ CALL_F_TRUE);
    }
    
    @Test
    public void casePatternVar() {
        assertEvalTrue("data Foo = Bar(Bool); def Bool f(Foo x) = case x { Bar(y) => y; }; { Bool testresult = f(Bar(True)); }");
    }

    
    
}
