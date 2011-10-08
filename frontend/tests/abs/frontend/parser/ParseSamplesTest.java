/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.parser;

import org.junit.Test;

import abs.frontend.FrontendTest;

public class ParseSamplesTest extends FrontendTest {

    @Test
    public void testSamples() throws Exception {
        String dir = "examples/";
        String[] files = { "PeerToPeer.abs", "PingPong.abs", "BoundedBuffer.abs"};
        for (int i = 0; i < files.length; i++) {
            // System.out.println("parsing " + files[i]);
            assertParseFileOk(dir + files[i], true);
        }
        
        assertParseFileOk("tests/abssamples/LexicalTest.abs", true);
    }

}
