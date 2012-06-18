/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java;

import org.junit.Test;

import abs.frontend.ast.Model;
import abs.frontend.parser.ParseSamplesTest;
import static abs.ABSTest.Config.*;
import static abs.backend.java.JavaBackendTest.*;

public class JavaExamplesTests extends ParseSamplesTest {

    public JavaExamplesTests(String input) {
        super(input);
    }

    @Test @Override
    public void test() throws Exception {
        super.test();
        assertValidJava(getJavaCode(m));
    }

    @Override
    protected Model parse(String input) throws Exception {
        return assertParseFileOk(input, WITH_STD_LIB, TYPE_CHECK);
    }
}
