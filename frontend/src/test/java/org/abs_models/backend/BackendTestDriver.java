/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend;

import java.io.File;

import org.abs_models.frontend.ast.Model;

public interface BackendTestDriver {

    public static enum BackendName {
        JAVA, ERLANG, MAUDE
    }

    public abstract BackendName getBackendName();

    public abstract boolean supportsTimedAbs();

    public abstract boolean supportsCustomSchedulers();
    
    public abstract boolean supportsExceptions();
    
    public abstract boolean supportsDowncasting();

    public abstract boolean supportsSQLite();

    public abstract void assertEvalEquals(String absCode, boolean value) throws Exception;

    public abstract void assertEvalFails(String absCode) throws Exception;

    public abstract void assertEvalTrue(String absCode) throws Exception;

    public abstract void assertEvalTrue(Model m) throws Exception;

    /**
     * Execute model m, making all files f available to the running model in a
     * backend-specific manner.  Currently used to provision an SQLite3
     * database to unit tests.
     *
     * @see org.abs_models.backend.common.SemanticTests#assertEvalTrueWithTestfiles(File, File...)
     */
    public abstract void assertEvalTrueWithTestfiles(Model m, File ...f) throws Exception;
}
