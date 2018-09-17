/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java;

import org.abs_models.frontend.typesystem.ExamplesTypeChecking;
import org.junit.Assume;
import org.junit.Test;

import static org.abs_models.backend.java.JavaBackendTest.*;

public class JavaExamplesTests extends ExamplesTypeChecking {

    public JavaExamplesTests(String input, String product) {
        super(input,product);
    }

    @Override
    protected void onError(String err) {
        Assume.assumeTrue(err, false);
    }

    @Test @Override
    public void test() throws Exception {
        super.test();
        assertValidJava(getJavaCode(m));
    }

}
