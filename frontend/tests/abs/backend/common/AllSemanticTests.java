/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.common;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;

@RunWith(Suite.class)
@Suite.SuiteClasses({ PrimitiveTypes.class, StmtTests.class, ObjectTests.class, FunctionalTests.class,
        ConcurrencyTests.class, StdLibTests.class, ModuleSystemTests.class })
public class AllSemanticTests {

}
