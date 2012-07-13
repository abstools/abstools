/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;

import abs.backend.common.*;
import abs.backend.java.*;
import abs.backend.maude.MaudeTests;
import abs.backend.prettyprint.PrettyPrinterTests;
import abs.backend.tests.ASTBasedABSTestRunnerGeneratorTest;

@RunWith(Suite.class)
@Suite.SuiteClasses({ AllJavaTests.class, AllSemanticTests.class, MaudeTests.MaudeEqTests.class, MaudeTests.MaudeRlTests.class, ASTBasedABSTestRunnerGeneratorTest.class, PrettyPrinterTests.class})
public class AllBackendTests {
}
