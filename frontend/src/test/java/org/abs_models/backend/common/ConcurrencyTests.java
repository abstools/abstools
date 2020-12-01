/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.common;

import java.io.File;

import org.abs_models.backend.BackendTestDriver;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;

@RunWith(Parameterized.class)
public class ConcurrencyTests extends SemanticTests {
    public ConcurrencyTests(BackendTestDriver d) {
        super(d);
    }

    static String INTERFACE_I = "interface I { Bool m(); Unit n(); } ";
    static String CLASS_C = "class C implements I { Unit n() { } Bool m() { return True; } } ";
    static String CLASS_D = "interface DI { } class D implements DI { { Bool b = False; } }";
    static String CALL_M_ASYNC = "{ Bool testresult = True; I i; i = new local C(); i!m(); }";
    static String COG_CALL_M_ASYNC = "{ Bool testresult = True; Fut<Bool> f; I i; i = new C(); f = i!m(); testresult = f.get; }";

    @Test
    public void asyncCall() throws Exception {
        assertEvalTrue(INTERFACE_I + CLASS_C + CALL_M_ASYNC);
    }

    static String CALL_M_ASYNC_GET = "{ Bool testresult = False; I i; i = new C(); Fut<Bool> fut; fut = i!m(); testresult = fut.get; }";

    @Test
    public void futGet() throws Exception {
        assertEvalTrue(INTERFACE_I + CLASS_C + CALL_M_ASYNC_GET);
    }

    static String CALL_M_ASYNC_AWAIT_GET = "{ Bool testresult = False; I i; i = new local C(); Fut<Bool> fut; fut = i!m(); await fut?; testresult = fut.get; }";

    @Test
    public void futAwaitAndGet() throws Exception {
        assertEvalTrue(INTERFACE_I + CLASS_C + CALL_M_ASYNC_AWAIT_GET);
    }

    @Test
    public void twoFutureGuard() throws Exception {
        assertEvalTrue(INTERFACE_I + CLASS_C + "{ Bool testresult = True; I i; i = new local C(); "
                + "  Fut<Bool> f1; Fut<Bool> f2;" + "  f1 = i!m(); f2 = i!m();" + "  await f1? & f2?; }");
    }

    @Test
    public void booleanGuard() throws Exception {
        assertEvalTrue(INTERFACE_I
                + "class C implements I { Bool b = False; Unit n() { b = True; } Bool m() { await b; return b; } }"
                + "{ I i; i = new C(); Fut<Bool> f; f = i!m(); i!n(); Bool testresult = False; testresult = f.get; } ");
    }

    // Future passing tests
    static String INTERFACE_IF = "interface I {Fut<Bool> m();}";
    static String CLASS_CF = "class C implements I {Bool n() {return True;} Fut<Bool> m() {Fut<Bool> p; p = this!n(); return p;}}";
    static String CLASS_CF2 = "class C implements I {Bool n() {return True;} Fut<Bool> m() { return this!n();}}";
    static String CLASS_CF3 = "class C implements I {Bool n() {return True;} Fut<Bool> inm(Fut<Bool> p) {suspend; return p;} Fut<Bool> m() {Fut<Fut<Bool>> result; {Fut<Bool> p; p = this!n(); result = this!inm(p);} await result?; return result.get;}}";
    static String CALL_M_FUTURE = "{Bool testresult = False; I i; i = new local C(); Fut<Bool> p; p = i.m(); await p?; testresult = p.get;}";

    @Test
    public void futureReturnValue() throws Exception {
        // Return a future variable
        assertEvalTrue(INTERFACE_IF + CLASS_CF + CALL_M_FUTURE);
    }

    @Test
    public void futureReturnCallResult() throws Exception {
        // Return the result of an asynchronous call
        assertEvalTrue(INTERFACE_IF + CLASS_CF2 + CALL_M_FUTURE);
    }

    @Test
    public void futureAsParameter() throws Exception {
        // Use a future after variable bindings at the call's location have gone
        // out of scope
        assertEvalTrue(INTERFACE_IF + CLASS_CF3 + CALL_M_FUTURE);
    }

    @Test
    public void await_field_future() throws Exception {
        assertEvalTrue(new File("abssamples/backend/ConcurrencyTests/await_field_future.abs"));
    }

    @Test
    public void initBlockCOG() throws Exception {
        assertEvalTrue(INTERFACE_I
                + "class C implements I { Bool b = False; { b = True; } Unit n() { } Bool m() { return b; }} "
                + COG_CALL_M_ASYNC);
    }

    @Test
    public void initBlockCOG2() throws Exception {
        assertEvalTrue(INTERFACE_I+CLASS_D
                + "class C implements I { Bool b = False; { b = True; DI i = new local D(); } Unit n() { } Bool m() { return b; }} "
                + COG_CALL_M_ASYNC);
    }
    
    @Test
    public void runMethodCOG() throws Exception {
        assertEvalTrue(INTERFACE_I
                + "class C implements I { Bool b = False; Unit run() { b = True; } Unit n() { } Bool m() { await b == True; return b; }} "
                + COG_CALL_M_ASYNC);
    }
    
    @Test
    public void initBlockCOG3() throws Exception {
        assertEvalTrue(INTERFACE_I+CLASS_C
                +"{ Bool testresult = False; List<Fut<Unit>> fs = Nil; Fut<Unit> f; I a = new C(); f = a!n(); fs = Cons(f,fs); f = a!n(); f = head(fs); f.get; testresult = True;}");
    }
    
    // ERROR Tests

    static String CALL_M_ASYNC_GET_DEADLOCK = "{ Bool testresult = False; I i; i = new local C(); Fut<Bool> fut; fut = i!m(); testresult = fut.get; }";

    @Test
    public void futGetDeadlock() throws Exception {
        // Fails because a deadlock occurs
        try {
            assertEvalFails(INTERFACE_I + CLASS_C + CALL_M_ASYNC_GET_DEADLOCK);
        } catch (java.util.concurrent.TimeoutException e) {
            // timeout is expected, do not fail
        }
    }

    static String CLASS_C_ASSERT_FAILS = "class C implements I { Unit n() { } Bool m() { assert False; return True; } } ";
    
    @Test
    public void assertionFails() throws Exception {
        assertEvalFails(INTERFACE_I + CLASS_C_ASSERT_FAILS + COG_CALL_M_ASYNC);
    }

    // Cross-cog sync calls are allowed until further notice
    // @Test
    // public void illegalSyncCall() throws Exception {
    //     // Fails because a synchronous call to an object of a different COG is not
    //     // allowed
    //     assertEvalFails(INTERFACE_I + CLASS_C
    //             + "{ Bool testresult = False; I i; i = new C(); testresult = i.m(); }");
    // }

    @Test
    public void ticket407_concise_await() throws Exception {
        assertEvalTrue(INTERFACE_I+CLASS_C+
                "{ I o = new local C(); Bool testresult = await o!m(); }");
    }
}
