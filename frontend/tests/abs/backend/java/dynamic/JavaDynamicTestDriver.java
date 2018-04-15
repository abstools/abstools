/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.dynamic;

import static org.junit.Assert.assertEquals;
import abs.backend.BackendTestDriver;
import abs.backend.java.codegeneration.JavaCode;
import abs.frontend.ast.Model;

public class JavaDynamicTestDriver implements BackendTestDriver {

    final JavaBackendDynamicTest javaTest;

    public JavaDynamicTestDriver() {
        javaTest = new JavaBackendDynamicTest();
    }

    @Override
    public String toString() {
        return "Dynamic Java";
    }

    @Override
    public void assertEvalEquals(String absCode, boolean value) throws Exception {
        javaTest.assertEvalEquals(absCode, value);
    }

    @Override
    public void assertEvalFails(String absCode) throws Exception {
        javaTest.assertEvalFails(absCode);
    }

    @Override
    public void assertEvalTrue(String absCode) throws Exception {
        assertEvalEquals(absCode, true);
    }

    @Override
    public void assertEvalTrue(Model m) throws Exception {
        JavaCode javaCode = javaTest.getJavaCodeDynamic(m);
        boolean res = javaTest.runJavaAndTestResult(javaCode, false);
        assertEquals(true, res);
    }

    @Override
    public BackendName getBackendName() {
        return BackendName.JAVA;
    }

    @Override
    public boolean supportsCustomSchedulers() { return false; }

    @Override
    public boolean supportsTimedAbs() { return false; }
}
