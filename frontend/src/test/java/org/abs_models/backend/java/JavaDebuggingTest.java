/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java;

import org.abs_models.backend.java.codegeneration.JavaCode;
import org.junit.Assert;
import org.junit.Ignore;
import org.junit.Test;

@Ignore
public class JavaDebuggingTest extends JavaBackendTest {

    @Test
    public void simpleStep() throws Exception {
        assertOutputContains("{ ; }", ":1");
    }

    @Test
    public void simpleStep2() throws Exception {
        assertOutputContains("{ \n ; }", ":2");
    }

    private void assertOutputContains(String absCode, String expectedOutput) throws Exception {
        JavaCode code = getJavaCode("module JavaTest;" + absCode);
        // System.out.println(java);
        String output = runJava(code, "-Dabs.debugger=" + TestDebugger.class.getName()).toString().trim();
        Assert.assertTrue("Expected to find " + expectedOutput + ", but output was:\n" + output, output.contains(expectedOutput));
    }

}
