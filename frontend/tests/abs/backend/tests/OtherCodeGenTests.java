/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.tests;

import org.junit.*;

import abs.backend.abs2haskell.ABS2HaskellDriver;
import abs.frontend.typesystem.CaseStudyTypeChecking;

public class OtherCodeGenTests extends CaseStudyTypeChecking {

    /* TODO: use random dir */
    public OtherCodeGenTests(String input) {
        super(input);
    }

    @Test
    public void testABS2Haskell() throws Exception {
        super.test();
        new ABS2HaskellDriver().generateAndCompile(m);
    }
}
