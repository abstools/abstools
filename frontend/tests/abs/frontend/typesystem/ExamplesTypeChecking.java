package abs.frontend.typesystem;

import org.junit.Test;

import abs.frontend.FrontendTest;

public class ExamplesTypeChecking extends FrontendTest {
    
    @Test
    public void abslang() {
        assertTypeCheckFileOk("src/abs/lang/abslang.abs");
    }

    @Test
    public void pingPong() {
        assertTypeCheckTestFileOk("pingpong.abs");
    }

    // not working yet
    public void peerToPeer() {
        assertTypeCheckTestFileOk("PeerToPeer.abs");
    }
    
    // not working yet
    public void boundedBuffer() {
        assertTypeCheckTestFileOk("boundedbuffer.abs");
    }

    private void assertTypeCheckTestFileOk(String fileName) {
        String dir="tests/abssamples/";
        assertTypeCheckFileOk(dir + fileName);
    }
    

}
