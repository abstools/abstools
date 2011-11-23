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
    public void abslang() throws Exception {
        assertTypeCheckFileOk("src/abs/lang/abslang.abs", false);
    }

    @Test
    public void pingPong() throws Exception {
        assertTypeCheckExampleFileOk("PingPong.abs");
    }

    @Test
    public void peerToPeer() throws Exception {
        assertTypeCheckExampleFileOk("PeerToPeer.abs");
    }

    @Test
    public void boundedBuffer() throws Exception {
        assertTypeCheckExampleFileOk("BoundedBuffer.abs");
    }

    @Test
    public void randomBool() throws Exception {
        assertTypeCheckTestFileOk("RandomBool.abs");
    }

    @Test
    public void lizeth() throws Exception {
        assertTypeCheckTestFileOk("lizeth.abs", false);
    }
    
    @Test
    public void fredhopper() throws Exception {
        assertTypeCheckTestFileOk("ReplicationSystem.abs");
    }

    private void assertTypeCheckExampleFileOk(String fileName) throws Exception {
        assertTypeCheckFileOk(EXAMPLEDIR+fileName, true);
    }
    
    private void assertTypeCheckTestFileOk(String fileName) throws Exception {
        assertTypeCheckTestFileOk(fileName, true);
    }

    private void assertTypeCheckTestFileOk(String fileName, boolean withStdLib) throws Exception {
        String dir = "tests/abssamples/";
        assertTypeCheckFileOk(dir + fileName, withStdLib);
    }

}
