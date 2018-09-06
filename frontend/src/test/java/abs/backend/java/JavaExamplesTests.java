/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java;

import org.junit.Assume;
import org.junit.Test;

import abs.frontend.typesystem.ExamplesTypeChecking;
import static abs.backend.java.JavaBackendTest.*;

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
