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
    
    static String INTERFACE_I = "interface I { Bool m(); Unit n(); } ";
    static String CLASS_C = "class C implements I { Unit n() { } Bool m() { return True; } } ";
    static String CALL_M_ASYNC = "{ Bool testresult = True; I i; i = new C(); i!m(); }";
    static String CALL_M_ASYNC_GET = "{ Bool testresult = False; I i; i = new cog C(); Fut<Bool> fut; fut = i!m(); testresult = fut.get; }";
    
    @Test
    public void asyncCall() {
       assertEvalTrue(INTERFACE_I+CLASS_C+CALL_M_ASYNC); 
    }
    
    @Test
    public void futGet() {
       assertEvalFails(INTERFACE_I+CLASS_C+CALL_M_ASYNC_GET); 
    }
    
    // ERROR Tests
    
    static String CALL_M_ASYNC_GET_DEADLOCK = "{ Bool testresult = False; I i; i = new C(); Fut<Bool> fut; fut = i!m(); testresult = fut.get; }";
    @Test
    public void futGetDeadlock() {
        // Fails because a deadlock occurs 
       assertEvalFails(INTERFACE_I+CLASS_C+CALL_M_ASYNC_GET_DEADLOCK); 
    }
    

}
