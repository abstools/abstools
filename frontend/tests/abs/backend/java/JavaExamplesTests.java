/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java;

import org.junit.Test;

public class JavaExamplesTests extends JavaBackendTest {

    private static final String EXAMPLEDIR = "examples/"; 
    
    @Test
    public void pingPong() throws Exception {
        assertValidJavaFile(EXAMPLEDIR+"PingPong.abs", true);
    }

    @Test
    public void peerToPeer() throws Exception {
        assertValidJavaFile(EXAMPLEDIR+"PeerToPeer.abs", true);
    }

    @Test
    public void boundedBuffer() throws Exception {
        assertValidJavaFile(EXAMPLEDIR+"BoundedBuffer.abs", true);
    }

    @Test
    public void randomBool() throws Exception {
        assertValidJavaExample("RandomBool.abs");
    }

    private void assertValidJavaExample(String fileName) throws Exception {
        assertValidExample(fileName, true);
    }

    private void assertValidExample(String fileName, boolean withStdLib) throws Exception {
        String dir = "tests/abssamples/";
        assertValidJavaFile(dir + fileName, withStdLib);
    }

}
