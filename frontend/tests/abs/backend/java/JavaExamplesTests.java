/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java;

import org.junit.Test;

import abs.frontend.FrontendTest;
import abs.frontend.ast.Model;

public class JavaExamplesTests extends JavaBackendTest {

    private static final String EXAMPLEDIR = "examples/"; 
    
    @Test
    public void pingPong() {
        assertValidJavaFile(EXAMPLEDIR+"PingPong.abs", true);
    }

    @Test
    public void peerToPeer() {
        assertValidJavaFile(EXAMPLEDIR+"PeerToPeer.abs", true);
    }

    @Test
    public void boundedBuffer() {
        assertValidJavaFile(EXAMPLEDIR+"BoundedBuffer.abs", true);
    }

    @Test
    public void randomBool() {
        assertValidJavaExample("RandomBool.abs");
    }

    @Test
    public void lizeth() {
        assertValidExample("lizeth.abs", false);
    }

    private void assertValidJavaExample(String fileName) {
        assertValidExample(fileName, true);
    }

    private void assertValidExample(String fileName, boolean withStdLib) {
        String dir = "tests/abssamples/";
        assertValidJavaFile(dir + fileName, withStdLib);
    }

}
