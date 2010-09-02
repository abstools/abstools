package abs.backend.java;

import junit.framework.Assert;

import org.junit.Test;

public class JavaObservationTest extends JavaBackendTest {

    @Test
    public void systemStarted() {
        assertOutputContains("{ }", "SYSTEM STARTED");
    }

    @Test
    public void systemTerminated() {
        assertOutputContains("{ }", "SYSTEM TERMINATED");
    }
    
    @Test
    public void objectCreated() {
        assertOutputContains("class C { } { new C(); }", "OBJECT CREATED: C");
    }
    
    private void assertOutputContains(String absCode, String expectedOutput) {
        String java = getJavaCode(absCode, false);
        String output = runJava(java, "-Dabs.systemobserver="+TestSystemObserver.class.getName()).toString().trim();
        if (output.contains(expectedOutput))
            Assert.assertTrue(true);
        else
            Assert.assertTrue("Expected to find "+expectedOutput+", but output was:\n"+output, false);
    }
    
}

