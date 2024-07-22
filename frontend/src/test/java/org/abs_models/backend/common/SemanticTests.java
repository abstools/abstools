/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.common;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;

import java.io.File;
import java.io.IOException;
import java.util.Collection;
import java.util.LinkedList;

import org.abs_models.ABSTest;
import org.abs_models.backend.BackendTestDriver;
import org.abs_models.backend.erlang.ErlangTestDriver;
import org.abs_models.backend.java.JavaTestDriver;
import org.abs_models.backend.maude.MaudeCompiler;
import org.abs_models.backend.maude.MaudeTestDriver;
import org.abs_models.frontend.ast.Model;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

@RunWith(Parameterized.class)
public abstract class SemanticTests {
    final protected BackendTestDriver driver;

    public SemanticTests(BackendTestDriver d) {
        driver = d;
    }

    public static boolean checkMaude() {
        return checkProg("maude");
    }

    public static boolean checkErlang() {
        return checkProg("erl");
    }

    public static boolean checkProg(String... prog) {
        ProcessBuilder pb = new ProcessBuilder();
        pb.command(prog);
        try {
            Process p = pb.start();
            p.destroy();
            return true;
        } catch (IOException e) {
            return false;
        }
    }

    @Parameters(name = "{0}")
    public static Collection<Object[]> data() {
        final Collection<Object[]> data = new LinkedList<>();
        /* TODO: Mark Maude tests as ignored instead of just missing them */
        /*
         * TODO: For the Java backend, we just use different RUNTIME options,
         * not code-gen options. So we could actually just compile the code to
         * Java once, and then run it with the different options.
         */
        data.add(new Object[] { new JavaTestDriver() });
        data.add(new Object[] { new JavaTestDriver(1) });
        /* TODO: Mark Maude tests as ignored instead of just missing them */
        // if (checkMaude()) {
        //     data.add(new Object[] { new MaudeTestDriver(MaudeCompiler.SIMULATOR.RL) });
        //     data.add(new Object[] { new MaudeTestDriver(MaudeCompiler.SIMULATOR.EQ_TIMED) });
        // }
        if (ErlangTestDriver.checkErlang()) {
            data.add(new Object[] { new ErlangTestDriver() });
        }
        // FIXME: enable after #302 is done, {new
        // JavaTestDriver(){{absArgs.add("-taskScheduler=simple");}} }
        return data;
    }

    public void assertEvalTrue(String absCode) throws Exception {
        driver.assertEvalTrue("module BackendTest; " + absCode);
    }

    public void assertEvalTrue(Model m) throws Exception {
        assertNotNull(m.lookupModule("BackendTest"));
        driver.assertEvalTrue(m);
    }

    /**
     * Runs the given test case, with the named files accessible from the model.
     *
     * The test runner will look for the files below the path
     * `frontend/src/test/resources/`.  Within the ABS test case, the files
     * named by f should be used without a directory prefix, since they will
     * all be copied to the same backend-specific location.
     *
     * @param model The file containing the ABS test case.
     * @param f The files to be made accessible.
     */
    public void assertEvalTrueWithTestfiles(File model, File ...f) throws Exception {
        Model m = ABSTest.assertParseFileOk(model.getPath());
        assertFalse(m.hasParserErrors());
        assertFalse(m.hasTypeErrors());
        assertNotNull(m.lookupModule("BackendTest"));
        driver.assertEvalTrueWithTestfiles(m, f);
    }

    public void assertEvalTrue(File f) throws Exception {
        Model m = ABSTest.assertParseFileOk(f.getPath());
        assertFalse(m.hasParserErrors());
        assertFalse(m.hasTypeErrors());
        assertEvalTrue(m);
    }

    public void assertEvalFails(String absCode) throws Exception {
        driver.assertEvalFails("module BackendTest; " + absCode);
    }
}
