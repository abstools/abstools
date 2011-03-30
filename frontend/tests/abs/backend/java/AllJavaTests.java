/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;

import abs.backend.common.AllSemanticTests;
import abs.backend.maude.MaudeTests;

@RunWith(Suite.class)
@Suite.SuiteClasses({ JavaPrimitiveTests.class, JavaStmtTests.class, JavaExprTests.class, JavaExamplesTests.class,
        JavaObservationTest.class })
public class AllJavaTests {

}
