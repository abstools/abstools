/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;

import abs.backend.common.AllSemanticTests;
import abs.backend.common.FunctionalTests;
import abs.backend.common.ObjectTests;
import abs.backend.common.SemanticTests;
import abs.backend.java.AllJavaTests;
import abs.backend.java.JavaStmtTests;
import abs.backend.java.JavaPrimitiveTests;
import abs.backend.maude.MaudeTests;
import abs.backend.tests.ABSTestRunnerGenerator;
import abs.backend.tests.ABSTestRunnerGeneratorTest;

@RunWith(Suite.class)
@Suite.SuiteClasses({ AllJavaTests.class, AllSemanticTests.class, MaudeTests.class, ABSTestRunnerGeneratorTest.class })
public class AllBackendTests {
}
