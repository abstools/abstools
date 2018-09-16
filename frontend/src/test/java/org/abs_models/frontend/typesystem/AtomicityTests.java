/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.frontend.typesystem;

import org.junit.Test;

import org.abs_models.frontend.FrontendTest;

public class AtomicityTests extends FrontendTest {

    @Test
    public void awaitStmt() {
        checkStmt("await True");
    }
    
    @Test
    public void suspendStmt() {
        checkStmt("suspend");
    }

    @Test
    public void syncCallStmt() {
        checkStmt("this.n(); this.atomN()");
    }

    @Test
    public void getExp() {
        checkStmt("Fut<Unit> f; f.get");
    }
    
    @Test
    public void overriding() {
        checkOverride("[Atomic]", "[Atomic]", false);
        checkOverride("[Atomic]", "", true);
        checkOverride("", "[Atomic]", true);
        
    }

    public void checkOverride(String a1, String a2, boolean fail) {
        String code = "interface I { "+a1+" Unit m(); } class C implements I { "+a2+" Unit m() { } }";
        if (fail) {
            assertTypeErrors(code);
        } else {
            assertTypeOK(code);
        }
    }
    
    public void checkStmt(String s) {
        assertTypeErrors("class C { [Atomic] Unit m() { "+s+"; } Unit n() { } [Atomic] Unit atomN() { }}");
        assertTypeOK("class C { Unit m() { "+s+"; } Unit n() { } Unit atomN() { }}");
    }
    
}
