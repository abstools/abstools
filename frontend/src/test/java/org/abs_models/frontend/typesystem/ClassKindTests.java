/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.frontend.typesystem;

import org.junit.Test;

import org.abs_models.frontend.FrontendTest;

public class ClassKindTests extends FrontendTest {

    @Test
    public void cogKind() {
        assertTypeErrors("interface I {} [COG] class C implements I {} { I i; i = new local C(); }");
        assertTypeOK("interface I {} [COG] class C implements I {} { I i; i = new C(); }");
    }
    
    @Test
    public void plainKind() {
        assertTypeOK("interface I {} [Plain] class C implements I {} { I i; i = new local C(); }");
        assertTypeErrors("interface I {} [Plain] class C implements I {} { I i; i = new C(); }");
    }

    
}
