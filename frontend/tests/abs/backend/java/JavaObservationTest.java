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

    @Test
    public void fieldValue() {
        assertOutputContains("class FieldClass { String field = \"HELLO WORLD\"; } { new FieldClass(); }", "FIELD VALUE=HELLO WORLD");
    }

    @Test
    public void fieldValue2() {
        assertOutputContains("class FieldClass(String field) { } { new FieldClass(\"HELLO WORLD\"); }", "FIELD VALUE=HELLO WORLD");
    }

    static final String I_AND_C = "interface I { Unit m(); Unit n(String s); }" +
    		" class C implements I { Unit m() { } Unit n(String s) { }}";
    
    
    @Test
    public void taskCreation() {
        assertOutputContains(I_AND_C+" { I i; i = new C(); i!m();}", 
                "TASK CREATED");
    }

    @Test
    public void taskCreation2() {
        assertOutputContains(I_AND_C+" { I i; i = new C(); i!m(); i!n(\"HALLO\"); }", 
                "TASK CREATED");
    }
    
    static final String STDDATA = "data Unit; data Fut<A>; data String;"; 
    
    private void assertOutputContains(String absCode, String expectedOutput) {
        String java = getJavaCode(STDDATA+absCode, false);
        //System.out.println(java);
        String output = runJava(java, "-Dabs.systemobserver="+TestSystemObserver.class.getName()).toString().trim();
        if (output.contains(expectedOutput))
            Assert.assertTrue(true);
        else
            Assert.assertTrue("Expected to find "+expectedOutput+", but output was:\n"+output, false);
    }
    
}

