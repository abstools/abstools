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
    public void letExp() {
       assertEvalTrue("def Bool f() = let (Bool x) = True in x;" + CALL_F); 
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
    
}
