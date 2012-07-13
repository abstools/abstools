/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;

@RunWith(Suite.class)
@Suite.SuiteClasses({
// ScannerTest.class,
        abs.backend.AllBackendTests.class, abs.frontend.AllFrontendTests.class })
public class AllTests {

}
