package abs.backend.java;

import abs.backend.BackendTestDriver;

public class JavaTestDriver extends BackendTestDriver {

    JavaBackendTest javaTest = new JavaBackendTest();

    @Override
    public void assertEvalEquals(String absCode, boolean value) {
        javaTest.assertEvalEquals(absCode, value);
    }

    @Override
    public void assertEvalFails(String absCode) {
        javaTest.assertEvalFails(absCode);
    }

}
