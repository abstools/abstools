/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java;

import static org.junit.Assert.assertEquals;
import abs.backend.BackendTestDriver;
import abs.backend.java.codegeneration.JavaCode;
import abs.frontend.ast.Model;

public class JavaTestDriver implements BackendTestDriver {

    final JavaBackendTest javaTest;

    public JavaTestDriver(long randomSeed) {
        javaTest = new JavaBackendTest(randomSeed);
    }
    
    public JavaTestDriver() {
        javaTest = new JavaBackendTest();
    }

    /**
     * Used by JUnit.
     */
    @Override
    public String toString() {
        return "JavaBackend"+ ( javaTest.seed == JavaBackendTest.seed_UNUSED ?  "" : (" seed="+Long.toString(javaTest.seed)));
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
        JavaCode javaCode = javaTest.getJavaCode(m);
        boolean res = javaTest.runJavaAndTestResult(javaCode, false);
        assertEquals(true, res);
    }

    @Override
    public boolean hasRollbacks() {
        return false;
    }

}
