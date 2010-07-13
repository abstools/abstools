package abs.frontend.typesystem;

import org.junit.Test;

import abs.frontend.FrontendTest;

public class ExamplesTypeChecking extends FrontendTest {
    
    @Test
    public void testSamples() {
        String dir="tests/abssamples/";     
        String[] files = {"PeerToPeer.abs", "pingpong.abs", "boundedbuffer.abs"};
        for (int i = 0 ; i < files.length; i++){
//          System.out.println("parsing " + files[i]);
            assertTypeCheckFileOk(dir + files[i]);
        }   
    }
    

}
