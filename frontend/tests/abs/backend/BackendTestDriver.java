/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend;

public abstract class BackendTestDriver {

    public abstract void assertEvalEquals(String absCode, boolean value);

    public abstract void assertEvalFails(String absCode);

    public void assertEvalTrue(String absCode) {
        assertEvalEquals(absCode, true);
    }
}
