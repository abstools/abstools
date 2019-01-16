/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java;

import org.junit.Assert;
import org.junit.Ignore;
import org.junit.Test;

import abs.backend.java.codegeneration.JavaCode;

@Ignore
public class JavaObservationTest extends JavaBackendTest {

    @Test
    public void systemStarted() throws Exception {
        assertOutputContains("{ }", "SYSTEM STARTED");
    }

    @Test
    public void mainTerminated() throws Exception {
        assertOutputContains("{ }", "MAIN TERMINATED");
    }

    @Test
    public void systemTerminated() throws Exception {
        assertOutputContains("{ }", "SYSTEM FINISHED");
    }
    
    @Test
    public void objectCreated() throws Exception {
        assertOutputContains("class C { } { new local C(); }", "OBJECT CREATED: C");
    }

    @Test
    public void fieldValue() throws Exception {
        assertOutputContains("class FieldClass { String field = \"HELLO WORLD\"; } { new local FieldClass(); }",
                "FIELD VALUE=HELLO WORLD");
    }

    @Test
    public void fieldValue2() throws Exception {
        assertOutputContains("class FieldClass(String field) { } { new local FieldClass(\"HELLO WORLD\"); }",
                "FIELD VALUE=HELLO WORLD");
    }

    static final String I_AND_C = "interface I { Unit m(); Unit n(String s); Unit k(Foo m);}"
            + " class C implements I { Unit m() { } Unit n(String s) { } Unit k(Foo m) { }}";

    @Test
    public void taskCreation() throws Exception {
        assertOutputContains(I_AND_C + " { I i; i = new local C(); i!m();}", "TASK CREATED");
    }

    @Test
    public void taskCreation2() throws Exception {
        assertOutputContains(I_AND_C + " { I i; i = new local C(); i!m(); i!n(\"HALLO\"); }", "TASK CREATED");
    }

    @Test
    public void taskCreation3() throws Exception {
        assertOutputContains(I_AND_C + "  { I i; i = new local C(); i!m(); i!k(Bar(\"HALLO\",\"Welt\")); }", "TASK CREATED");
    }

    static final String STDDATA = "data Foo = Bar(String,String);";

    private void assertOutputContains(String absCode, String expectedOutput) throws Exception {
        JavaCode code = getJavaCode("module JavaTest;" + STDDATA + absCode, Config.WITH_STD_LIB);
        // System.out.println(java);
        String output = runJava(code, "-Dabs.systemobserver=" + TestSystemObserver.class.getName()).toString().trim();
        Assert.assertTrue("Expected to find " + expectedOutput + ", but output was:\n" + output, output.contains(expectedOutput));
    }

}
