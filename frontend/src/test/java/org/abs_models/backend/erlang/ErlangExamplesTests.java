/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.erlang;

import org.abs_models.frontend.typesystem.ExamplesTypeChecking;
import org.junit.Assume;
import org.junit.BeforeClass;
import org.junit.Test;

public class ErlangExamplesTests extends ExamplesTypeChecking {

    public ErlangExamplesTests(String input, String product) {
        super(input, product);
    }

    @BeforeClass
    public static void checkRequired() {
        Assume.assumeTrue(ErlangTestDriver.checkErlang());
    }

    @Override
    protected void onError(String err) {
        Assume.assumeTrue(err, false);
    }

    @Test
    @Override
    public void test() throws Exception {
        super.test();
        new ErlangTestDriver().generateAndCompile(m);
    }

}
