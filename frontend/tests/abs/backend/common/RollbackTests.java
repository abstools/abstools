/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.common;

import org.junit.Test;

import abs.backend.BackendTestDriver;

public class RollbackTests extends SemanticTests {

    public RollbackTests(BackendTestDriver d) {
        super(d);
    }

    @Test
    public void rollback1() {
        assertEvalTrue("interface I { Unit test(); Bool getR(); }\n"
                +      "class C implements I { Bool r = True; Unit test() { r = False; abort \"\"; } Bool getR() { return this.r;}}\n"
                +       "{ I o = new C(); Fut<Unit> f = o!test(); await f?; Bool testresult = o.getR(); }");
    }
}
