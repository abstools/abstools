/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.common;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;

import abs.backend.BackendTestDriver;

@RunWith(Parameterized.class)
public class DeltaTests extends SemanticTests {
    public DeltaTests(BackendTestDriver d) {
        super(d);
    }

    static String INTERFACE_I = "interface I { Int m(); } ";
    static String CLASS_C = "class C implements I { Int m() { return 1; } } ";
    static String DELTA_D = "delta D; modifies class BackendTest.C {modifies Int m() { return 2; }} ";
    static String CALL_ORIGINAL = "{Bool testresult = False; Int s = 0; I i; i = new local C(); s = i.m(); testresult = s == 1;}";
    static String CALL_DELTA = "{Bool testresult = False; Int s = 0; I i; i = new local C(); s = i.m(); testresult = s == 2;}";

    @Test
    public void nonAppliedDelta() {
        assertEvalTrue(INTERFACE_I + CLASS_C + CALL_ORIGINAL + DELTA_D);
    }
    // TODO: design a way to give arguments to assertEvalTrue (we will need this
    // when selecting among products as well, later).
}
