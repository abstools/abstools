package abs.backend.java;

import org.junit.Test;

import abs.frontend.FrontendTest;
import abs.frontend.ast.Model;

public class JavaExamplesTests extends JavaBackendTest {
    
    @Test
    public void pingPong() {
        assertValidJavaFile("pingpong.abs");
    }

    @Test 
    public void peerToPeer() {
        assertValidJavaFile("PeerToPeer.abs");
    }
    
    @Test
    public void boundedBuffer() {
        assertValidJavaFile("boundedbuffer.abs");
    }

    @Test
    public void randomBool() {
        assertValidJavaFile("RandomBool.abs");
    }
    
    private void assertValidJavaFile(String fileName) {
        String dir="tests/abssamples/";
        assertValidJavaFile(dir + fileName, true);
    }
    
}
