/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java;

import abs.backend.BackendTestDriver;

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

}
