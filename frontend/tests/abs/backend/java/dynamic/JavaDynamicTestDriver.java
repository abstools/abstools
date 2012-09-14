/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.dynamic;

import abs.backend.BackendTestDriver;

public class JavaDynamicTestDriver implements BackendTestDriver {

    final JavaBackendDynamicTest javaTest;

    public JavaDynamicTestDriver() {
        javaTest = new JavaBackendDynamicTest();
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
