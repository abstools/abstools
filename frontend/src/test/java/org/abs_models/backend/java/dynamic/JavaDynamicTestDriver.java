/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java.dynamic;

import static org.junit.Assert.assertEquals;

import java.io.File;

import org.abs_models.backend.BackendTestDriver;
import org.abs_models.backend.java.codegeneration.JavaCode;
import org.abs_models.common.NotImplementedYetException;
import org.abs_models.frontend.ast.Model;

public class JavaDynamicTestDriver implements BackendTestDriver {

    final JavaBackendDynamicTest javaTest;

    public JavaDynamicTestDriver() {
        javaTest = new JavaBackendDynamicTest();
    }

    @Override
    public String toString() {
        return "Dynamic Java";
    }

    @Override
    public void assertEvalEquals(String absCode, boolean value) throws Exception {
        javaTest.assertEvalEquals(absCode, value);
    }

    @Override
    public void assertEvalFails(String absCode) throws Exception {
        javaTest.assertEvalFails(absCode);
    }

    @Override
    public void assertEvalTrue(String absCode) throws Exception {
        assertEvalEquals(absCode, true);
    }

    @Override
    public void assertEvalTrue(Model m) throws Exception {
        JavaCode javaCode = javaTest.getJavaCodeDynamic(m);
        boolean res = javaTest.runJavaAndTestResult(javaCode, false);
        assertEquals(true, res);
    }

    @Override
    public void assertEvalTrueWithTestfiles(Model m, File ...f) throws Exception {
        throw new Exception("Auxiliary files not supported in Java test backend.");
    }

    @Override
    public BackendName getBackendName() {
        return BackendName.JAVA;
    }

    @Override
    public boolean supportsCustomSchedulers() { return false; }

    @Override
    public boolean supportsTimedAbs() { return false; }

    @Override
    public boolean supportsExceptions() { return false; }

    @Override
    public boolean supportsSQLite() { return false; }

    @Override
    public boolean supportsModelApi() {
        return false;
    }
}
