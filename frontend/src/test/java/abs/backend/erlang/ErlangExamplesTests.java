/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.erlang;

import org.junit.Assume;
import org.junit.BeforeClass;
import org.junit.Test;

import abs.frontend.typesystem.ExamplesTypeChecking;

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
