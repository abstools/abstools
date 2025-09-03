/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend;

import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;

import org.abs_models.backend.common.InternalBackendException;
import org.abs_models.common.WrongProgramArgumentException;
import org.abs_models.frontend.ast.Model;
import org.abs_models.frontend.delta.DeltaModellingException;

public interface BackendTestDriver {

    public static enum BackendName {
        JAVA, ERLANG, MAUDE
    }

    public abstract BackendName getBackendName();

    public abstract boolean supportsTimedAbs();

    public abstract boolean supportsCustomSchedulers();

    public abstract boolean supportsExceptions();

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

    public abstract boolean supportsModelApi();
    default void startModelApi(File file) throws IOException, DeltaModellingException, WrongProgramArgumentException, InternalBackendException, InterruptedException { }
    default void shutdownModelApi() { }
    default String sendGetRequest(String request, int expectedResponseCode) throws IOException, URISyntaxException { return null; }
    default String sendPostRequest(String request, String jsonPayload, int expectedResponseCode) throws IOException, URISyntaxException { return null; }
}
