/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.typesystem;

import org.junit.Test;

import abs.frontend.FrontendTest;

public class ExamplesTypeChecking extends FrontendTest {

    private static final String EXAMPLEDIR = "examples/"; 
    
    @Test
    public void abslang() {
        assertTypeCheckFileOk("src/abs/lang/abslang.abs", false);
    }

    @Test
    public void pingPong() {
        assertTypeCheckExampleFileOk("PingPong.abs");
    }

    @Test
    public void peerToPeer() {
        assertTypeCheckExampleFileOk("PeerToPeer.abs");
    }

    @Test
    public void boundedBuffer() {
        assertTypeCheckExampleFileOk("BoundedBuffer.abs");
    }

    @Test
    public void randomBool() {
        assertTypeCheckTestFileOk("RandomBool.abs");
    }

    @Test
    public void lizeth() {
        assertTypeCheckTestFileOk("lizeth.abs", false);
    }

    private void assertTypeCheckExampleFileOk(String fileName) {
        assertTypeCheckFileOk("examples/"+fileName, true);
    }
    
    private void assertTypeCheckTestFileOk(String fileName) {
        assertTypeCheckTestFileOk(fileName, true);
    }

    private void assertTypeCheckTestFileOk(String fileName, boolean withStdLib) {
        String dir = "tests/abssamples/";
        assertTypeCheckFileOk(dir + fileName, true);
    }

}
