/**

 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.erlang;

import static org.junit.Assert.assertEquals;
import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.EnumSet;

import org.abs_models.backend.common.InternalBackendException;
import org.abs_models.frontend.ast.*;
import org.apache.commons.io.FileUtils;
import org.junit.Assert;
import org.junit.Assume;

import org.abs_models.ABSTest;
import org.abs_models.backend.BackendTestDriver;
import org.abs_models.backend.common.SemanticTests;

import com.google.common.io.Files;

public class ErlangTestDriver extends ABSTest implements BackendTestDriver {

    @Override
    public String toString() {
        return "Erlang";
    }

    public static boolean checkErlang() {
        /* TODO: Should be checked earlier instead, before configuring the JUnit suites. */
        String doAbs = System.getProperty("abs.junit.erlang");
        Assume.assumeFalse("Erlang tests disabled via -Dabs.junit.erlang", "0".equals(doAbs));
        if (SemanticTests.checkProg("erl")) {
            try {
                Assume.assumeTrue("Need Erlang R" + Integer.toString(ErlangBackend.minErlangVersion) + " or later.",
                                  ErlangBackend.getErlangVersion() >= ErlangBackend.minErlangVersion);
            } catch (IOException e) {
                return false;
            } catch (InterruptedException e) {
                return false;
            }
        }
        return true;
    }

    @Override
    public void assertEvalEquals(String absCode, boolean value) throws Exception {
        if (value)
            assertEvalTrue(absCode);
        else
            assertEvalFails(absCode);
    }

    @Override
    public void assertEvalFails(String absCode) throws Exception {
        Assert.assertEquals(null, runAndCheck(absCode));
    }

    @Override
    public void assertEvalTrue(String absCode) throws Exception {
        Assert.assertEquals("True", runAndCheck(absCode));
    }

    /**
     * Parses, complies, runs given code and returns value of testresult.
     *
     * @param absCode
     * @return either the Result, or null if an execution error occurred
     */
    private String runAndCheck(String absCode) {
        File f = null;
        try {
            f = Files.createTempDir();
            f.deleteOnExit();
            Model model = assertParseOk(absCode, Config.WITH_STD_LIB, Config.TYPE_CHECK, /* XXX:CI Config.WITH_LOC_INF, */ Config.WITHOUT_MODULE_NAME);
            String mainModule = genCode(model, f, true);
            return run(f, mainModule);
        } catch (Exception e) {
            return null;
        } finally {
            try {
                FileUtils.deleteDirectory(f);
            } catch (IOException e) {
                // Ignore Ex, File should be deleted anyway
            }
        }
    }

    public void generateAndCompile(Model model) throws Exception {
        File f = null;
        try {
            f = Files.createTempDir();
            f.deleteOnExit();
            genCode(model, f, false);
        } finally {
            try {
                FileUtils.deleteDirectory(f);
            } catch (IOException e) {
                // Ignore Ex, File should be deleted anyway
            }
        }
    }

    /**
     * Generates Erlang code in target directory, adding a last statement that
     * prints the value of the `testresult' variable.
     *
     * @return a Module Name containing a Main Block
     * @throws InternalBackendException
     *
     */
    public String genCode(Model model, File targetDir, boolean appendResultprinter) throws IOException, InterruptedException, InternalBackendException {
        if (model.hasErrors()) {
            Assert.fail(model.getErrors().getFirstError().getHelpMessage());
        }
        if (model.hasTypeErrors()) {
            Assert.fail(model.getTypeErrors().getFirstError().getHelpMessage());
        }
        MainBlock mb = model.getMainBlock();
        if (mb != null && appendResultprinter) {
            // We search for this output in the `run' method below
            mb.addStmt(new ExpressionStmt(
                new List<>(),
                new FnApp("ABS.StdLib.println",
                    new List<>(new AddAddExp(new StringLiteral("RES="),
                        new FnApp("ABS.StdLib.toString",
                            new List<>(new VarUse("testresult"))))))));
        }
        new ErlangBackend().compile(model, targetDir,
// use the following argument for silent compiler:
                              EnumSet.noneOf(ErlangBackend.CompileOptions.class)
// use the following argument for verbose compiler output:
                              // EnumSet.of(ErlangBackend.CompileOptions.VERBOSE)
                              );
        if (mb == null)
            return null;
        else
            return mb.getModuleDecl().getName();

    }

    /**
     * Executes mainModule
     *
     * To detect faults, we have a Timeout process which will kill the
     * runtime system after 10 seconds
     */
    private String run(File workDir, String mainModule) throws Exception {
        String val = null;
        String runFileName = ErlangBackend.isWindows() ? "run.bat" : "run";
        File runFile = new File(workDir, runFileName);
        ProcessBuilder pb = new ProcessBuilder(runFile.getAbsolutePath(), mainModule);
        pb.directory(workDir);
        pb.redirectErrorStream(true);
        Process p = pb.start();

        Thread t = new Thread(new TimeoutThread(p));
        t.start();
        // Search for result
        BufferedReader br = new BufferedReader(new InputStreamReader(p.getInputStream()));
        while (true) {
            String line = br.readLine();
            if (line == null)
                break;
            if (line.startsWith("RES=")) // see `genCode' above
                val = line.split("=")[1];
        }
        int res = p.waitFor();
        t.interrupt();
        if (res != 0)
            return null;
        return val;

    }

    @Override
    public void assertEvalTrue(Model model) throws Exception {
        File f = null;
        try {
            f = Files.createTempDir();
            f.deleteOnExit();
            String mainModule = genCode(model, f, true);
            assertEquals("True",run(f, mainModule));
        } finally {
            try {
                FileUtils.deleteDirectory(f);
            } catch (IOException e) {
                // Ignore Ex, File should be deleted anyway
            }
        }
    }

    @Override
    public BackendName getBackendName() {
        return BackendName.ERLANG;
    }

    @Override
    public boolean supportsCustomSchedulers() { return true; }

    @Override
    public boolean supportsTimedAbs() { return true; }

    @Override
    public boolean supportsExceptions() { return true; }

    @Override
    public boolean supportsDowncasting() { return true; }
}

class TimeoutThread implements Runnable {

    private final Process p;

    public TimeoutThread(Process p) {
        super();
        this.p = p;
    }

    @Override
    public void run() {
        try {
            Thread.sleep(10000);
            p.destroy();
        } catch (InterruptedException e) {
        }
    }
}
