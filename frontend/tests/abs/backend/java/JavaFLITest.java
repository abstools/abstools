/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java;

import org.junit.Test;

public class JavaFLITest extends JavaBackendTest {

    @Test
    public void foreignClass() {
        assertValidStdLib("import * from ABS.FLI; interface I { Unit foo(String s); } " +
                    "[Foreign] class A implements I { Unit foo(String s) { } }");
    }

}
