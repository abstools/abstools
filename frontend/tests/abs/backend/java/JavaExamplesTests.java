/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
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
        assertValidJavaFile("BoundedBuffer.abs");
    }

    @Test
    public void randomBool() {
        assertValidJavaFile("RandomBool.abs");
    }

    @Test
    public void lizeth() {
        assertValidExample("lizeth.abs", false);
    }

    private void assertValidJavaFile(String fileName) {
        assertValidExample(fileName, true);
    }

    private void assertValidExample(String fileName, boolean withStdLib) {
        String dir = "tests/abssamples/";
        assertValidJavaFile(dir + fileName, withStdLib);
    }

}
