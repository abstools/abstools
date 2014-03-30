/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.common;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;

@RunWith(Suite.class)
@Suite.SuiteClasses({ 
    ConcurrencyTests.class, 
        // DeltaTests.class, // haskell backend does not plan to support deltas
    FunctionalTests.class,
    ModuleSystemTests.class, 
    ObjectTests.class, 
    PrimitiveTypes.class, 
    StdLibTests.class, 
    StmtTests.class, 
    TimeTests.class })
public class AllSemanticTests {

}
