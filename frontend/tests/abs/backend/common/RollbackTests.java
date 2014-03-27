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

    // TODO: Here's sync-vs-asynch confusion!

    @Test
    public void rollback1() {
        assertEvalTrue("interface I { Unit test(); Bool getR(); }\n"
                + "class C implements I { Bool r = True; Unit test() { r = False; abort \"\"; } Bool getR() { return this.r;}}\n"
                + "{ I o = new C(); Fut<Unit> f = o!test(); await f?; Bool testresult = await o!getR(); }");
    }

    @Test
    public void rollbackOverCall1() {
        assertEvalTrue("interface I { Unit test(Bool fail); Bool getR(); }\n"
                + "class C implements I { Bool r = True; Unit test(Bool fail) { I o2 = new local C(); r = False; if (fail) abort \"\"; else o2.test(True); } Bool getR() { return this.r;}}\n"
                + "{ I o = new C(); Fut<Unit> f = o!test(False); await f?; Bool testresult = await o!getR(); }");
    }

    void rollbackOverCall(boolean doAwait) {
        assertEvalTrue("interface I { Unit test(Bool fail); Bool getR(); }\n"
                + "class C implements I { Bool r = True; Unit test(Bool fail) { I o2 = new C(); r = True; if (fail) abort \"\"; else "+(doAwait ? "await" : "")+" o2!test(True); } Bool getR() { return this.r;}}\n"
                + "{ I o = new C(); Fut<Unit> f = o!test(False); await f?; Bool testresult = await o!getR(); }");
    }

    @Test
    public void rollbackOverCall2() { rollbackOverCall(false); }

    @Test
    // Aborting while awaiting shouldn't change a thing.
    public void rollbackOverCall3() { rollbackOverCall(true); }

    /* These two tests should create a runtime error WITHIN an Await.
     * We expect the local state to be rolled back.
     * - Maude doesn't generate runtime errors for that (yet)
     * - Erlang probably should?
     * We test:
     * - div-by-0
     * - unmatched pattern
     * TODO: returns null in Erlang
     */
    @Test
    public void rollbackRTE1() {
        assertEvalTrue("interface I { Unit test(); Bool getR(); }\n"
                +      "class C implements I { Bool r = True; Unit test() { r = False; await r && 1/0 > 0; } Bool getR() { return this.r;}}\n"
                +       "{ I o = new local C(); Fut<Unit> f = o!test(); await f?; Bool testresult = o.getR(); }");
    }

    @Test
    public void rollbackRTE2() {
        assertEvalTrue("interface I { Unit test(); Bool getR(); }\n"
                +      "class C implements I { Bool r = True; Unit test() { r = False; await r && 1/0 > 0; } Bool getR() { return this.r;}}\n"
                +       "{ I o = new C(); Fut<Unit> f = o!test(); await f?; Bool testresult = await o!getR(); }");
    }

    @Test
    public void rollbackRTE3() {
        assertEvalTrue("interface I { Unit test(); Bool getR(); }\n"
                +      "class C implements I { Bool r = True; Unit test() { r = False; await r && head(Nil); } Bool getR() { return this.r;}}\n"
                +       "{ I o = new C(); Fut<Unit> f = o!test(); await f?; Bool testresult = await o!getR(); }");
    }
}
