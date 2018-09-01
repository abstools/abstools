/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.parser;

import org.junit.Test;

import abs.frontend.FrontendTest;

public class TestABSPackages extends FrontendTest {

    @Test
    public void leaderElectionPackage() throws Exception {
       assertParseFileOk("abssamples/leaderelection.jar", true);
    }
}
