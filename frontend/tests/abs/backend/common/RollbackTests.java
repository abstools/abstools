/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.common;

import org.junit.Assume;
import org.junit.Test;

import abs.backend.BackendTestDriver;

public class RollbackTests extends SemanticTests {

    public RollbackTests(BackendTestDriver d) {
        super(d);
        Assume.assumeTrue(d.hasRollbacks());        
    }

    @Test
    public void rollback1() {
        assertEvalTrue("interface I { Unit test(); Bool getR(); }\n"
                + "class C implements I { Bool r = True; Unit test() { r = False; abort \"\"; } Bool getR() { return this.r;}}\n"
                + "{ I o = new C(); Fut<Unit> f = o!test(); await f?; Bool testresult = await o!getR(); }");
    }

    @Test
    public void rollbackOverCall1() {
        assertEvalTrue("interface I { Unit test(Bool fail); Bool getR(); }\n"
                + "class C implements I { Bool r = True; Unit test(Bool fail) { I o2 = new C(); r = False; if (fail) abort \"\"; else o2.test(True); } Bool getR() { return this.r;}}\n"
                + "{ I o = new C(); Fut<Unit> f = o!test(False); await f?; Bool testresult = await o!getR(); }");
    }

    @Test
    public void rollbackOverCall2() {
        assertEvalTrue("interface I { Unit test(Bool fail); Bool getR(); }\n"
                + "class C implements I { Bool r = True; Unit test(Bool fail) { I o2 = new C(); r = True; if (fail) abort \"\"; else o2!test(True); } Bool getR() { return this.r;}}\n"
                + "{ I o = new C(); Fut<Unit> f = o!test(False); await f?; Bool testresult = await o!getR(); }");
    }
    
    @Test
    public void rollbackOverCall3() {
        // Aborting while awaiting shouldn't change a thing.
        assertEvalTrue("interface I { Unit test(Bool fail); Bool getR(); }\n"
                +      "class C implements I { Bool r = True; Unit test(Bool fail) { I o2 = new C(); r = True; if (fail) abort \"\"; else await o2!test(True); } Bool getR() { return this.r;}}\n"
                +       "{ I o = new C(); Fut<Unit> f = o!test(False); await f?; Bool testresult = o.getR(); }");
    }

    @Test
    public void rollbackAwait() {
        // Maude doesn't generate runtime errors for that (yet)
        assertEvalTrue("interface I { Unit test(); Bool getR(); }\n"
                +      "class C implements I { Bool r = True; Unit test() { r = False; await r && 1/0 > 0; } Bool getR() { return this.r;}}\n"
                +       "{ I o = new C(); Fut<Unit> f = o!test(); await f?; Bool testresult = o.getR(); }");
    }
}
