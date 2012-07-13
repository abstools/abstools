/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;

import abs.backend.java.lib.net.AllNetTests;

@RunWith(Suite.class)
@Suite.SuiteClasses({ JavaPrimitiveTests.class, 
    JavaStmtTests.class, 
    JavaExprTests.class, 
    JavaExamplesTests.class,
    JavaObservationTest.class,
    JavaFLITest.class
    , AllNetTests.class
    , CaseStudies.class
    })
public class AllJavaTests {

}
