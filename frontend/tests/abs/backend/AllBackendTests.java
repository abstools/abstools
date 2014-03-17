/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;

import abs.backend.abs2haskell.ABS2HaskellExamplesTest;
import abs.backend.common.AllSemanticTests;
import abs.backend.tests.OtherCodeGenTests;

@RunWith(Suite.class)
@Suite.SuiteClasses({
    /* Do not merge */
    AllSemanticTests.class,
    OtherCodeGenTests.class,
    ABS2HaskellExamplesTest.class
        /* This line intentionally left blank */
        })
public class AllBackendTests {
}
