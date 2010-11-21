package abs.backend.java;

import org.junit.Test;

import junit.framework.Assert;

public class JavaDebuggingTest extends JavaBackendTest {

    @Test
    public void simpleStep() {
        assertOutputContains("{ ; }", ":1");
    }

    @Test
    public void simpleStep2() {
        assertOutputContains("{ \n ; }", ":2");
    }

    private void assertOutputContains(String absCode, String expectedOutput) {
        JavaCode code = getJavaCode("module JavaTest;" + absCode, true);
        // System.out.println(java);
        String output = runJava(code, "-Dabs.debugger=" + TestDebugger.class.getName()).toString().trim();
        if (output.contains(expectedOutput))
            Assert.assertTrue(true);
        else
            Assert.assertTrue("Expected to find " + expectedOutput + ", but output was:\n" + output, false);
    }

}
