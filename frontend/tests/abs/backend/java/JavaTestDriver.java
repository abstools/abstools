/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java;

import abs.backend.BackendTestDriver;

public class JavaTestDriver extends BackendTestDriver {

    JavaBackendTest javaTest;

    public JavaTestDriver(long randomSeed) {
        javaTest = new JavaBackendTest(randomSeed);
    }
    
    public JavaTestDriver() {
        javaTest = new JavaBackendTest();
    }
    
    @Override
    public void assertEvalEquals(String absCode, boolean value) {
        try {
            javaTest.assertEvalEquals(absCode, value);
        } catch (Exception e) {
            throw new RuntimeException(e); // TODO: throw
        }
    }

    @Override
    public void assertEvalFails(String absCode) {
        try {
            javaTest.assertEvalFails(absCode);
        } catch (Exception e) {
            throw new RuntimeException(e); // TODO: throw
        }
    }

}
