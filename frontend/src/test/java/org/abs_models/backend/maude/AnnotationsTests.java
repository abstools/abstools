/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.maude;

import org.junit.Test;

import org.abs_models.backend.maude.MaudeTests.MaudeEqTests;

/**
 * Pick one of the two; maybe later it may be necessary to try both if there are semantic effects.
 * @author stolz
 *
 */
public class AnnotationsTests extends MaudeEqTests {

    @Test
    public void test_Cost() throws Exception {
        assertTrueMaude("module Test; import * from ABS.DC; { DC dc = null; [DC: dc] skip; Bool testresult = True;}");
    }
}
