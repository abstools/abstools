/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend;

import abs.frontend.ast.Model;

public interface BackendTestDriver {

    public static enum BackendName {
        JAVA, ERLANG, MAUDE
    }

    public abstract BackendName getBackendName();

    public abstract boolean supportsTimedAbs();

    public abstract boolean supportsCustomSchedulers();
    
    public abstract boolean supportsExceptions();

    public abstract void assertEvalEquals(String absCode, boolean value) throws Exception;

    public abstract void assertEvalFails(String absCode) throws Exception;

    public abstract void assertEvalTrue(String absCode) throws Exception;

    public abstract void assertEvalTrue(Model m) throws Exception;
}
