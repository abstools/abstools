package abs.backend.common;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;

import abs.backend.BackendTestDriver;

@RunWith(Parameterized.class)
public class ConcurrencyTests extends SemanticTests {
    public ConcurrencyTests(BackendTestDriver d) {
        super(d);
    }
    
    static String INTERFACE_I = "interface I { Bool m(); Unit n(); }";
    static String CALL_M = "{ Bool testresult = False; I i; i = new C(); testresult = i.m(); }";
    
    @Test
    public void asyncCall() {
       assertEvalTrue(INTERFACE_I+" class C implements I { Unit n() { } Bool m() { I i; i = this; i!n(); return True; } } "+CALL_M); 
    }
    
    

}
