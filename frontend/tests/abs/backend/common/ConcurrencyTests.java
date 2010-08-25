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
    
    @Test
    public void asyncCall() {
       assertEvalTrue(INTERFACE_I+CLASS_C+CALL_M_ASYNC); 
    }
    
    static String CALL_M_ASYNC_GET = "{ Bool testresult = False; I i; i = new cog C(); Fut<Bool> fut; fut = i!m(); testresult = fut.get; }";
    @Test
    public void futGet() {
       assertEvalTrue(INTERFACE_I+CLASS_C+CALL_M_ASYNC_GET); 
    }

    static String CALL_M_ASYNC_AWAIT_GET = "{ Bool testresult = False; I i; i = new C(); Fut<Bool> fut; fut = i!m(); await fut?; testresult = fut.get; }";
    @Test
    public void futAwaitAndGet() {
       assertEvalTrue(INTERFACE_I+CLASS_C+CALL_M_ASYNC_AWAIT_GET); 
    }
    
    @Test
    public void booleanGuard() {
       assertEvalTrue(INTERFACE_I+"class C implements I { Bool b = False; Unit n() { b = True; } Bool m() { await b; return b; } }"+
               "{ I i; i = new cog C(); Fut<Bool> f; f = i!m(); i!n(); Bool testresult = False; testresult = f.get; } ");;  
    }

    
    
    // ERROR Tests
    
    static String CALL_M_ASYNC_GET_DEADLOCK = "{ Bool testresult = False; I i; i = new C(); Fut<Bool> fut; fut = i!m(); testresult = fut.get; }";

    @Test
    public void futGetDeadlock() {
        // Fails because a deadlock occurs 
       assertEvalFails(INTERFACE_I+CLASS_C+CALL_M_ASYNC_GET_DEADLOCK); 
    }
    
    @Test
    public void illegalSyncCall() {
        // Fails because a synchrous call to an object of a different COG is not allowed
       assertEvalFails(INTERFACE_I+CLASS_C+"{ Bool testresult = False; I i; i = new cog C(); testresult = i.m(); }"); 
    }

    
}
