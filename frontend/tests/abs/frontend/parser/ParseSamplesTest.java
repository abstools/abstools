//$Id$ 

package abs.frontend.parser;

import org.junit.Test;

import abs.frontend.FrontendTest;

public class ParseSamplesTest extends FrontendTest {

    @Test
    public void testSamples() {
        String dir = "tests/abssamples/";
        String[] files = { "PeerToPeer.abs", "pingpong.abs", "BoundedBuffer.abs" };
        for (int i = 0; i < files.length; i++) {
            // System.out.println("parsing " + files[i]);
            assertParseFileOk(dir + files[i], true);
        }
    }

}
