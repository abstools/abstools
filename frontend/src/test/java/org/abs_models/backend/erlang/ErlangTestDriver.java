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
import java.io.InputStream;
import java.io.Closeable;
import java.util.EnumSet;
import java.util.concurrent.TimeUnit;
import com.google.common.io.Files;

import org.abs_models.ABSTest;
import org.abs_models.backend.BackendTestDriver;
import org.abs_models.backend.common.InternalBackendException;
import org.abs_models.backend.common.SemanticTests;
import org.abs_models.frontend.ast.AddAddExp;
import org.abs_models.frontend.ast.ExpressionStmt;
import org.abs_models.frontend.ast.FnApp;
import org.abs_models.frontend.ast.List;
import org.abs_models.frontend.ast.MainBlock;
import org.abs_models.frontend.ast.Model;
import org.abs_models.frontend.ast.StringLiteral;
import org.abs_models.frontend.ast.VarUse;
import org.apache.commons.io.FileUtils;
import org.junit.Assert;
import org.junit.Assume;

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
            Model model = assertParse(absCode, Config.TYPE_CHECK, /* XXX:CI Config.WITH_LOC_INF, */ Config.WITHOUT_MODULE_NAME);
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

        try ( // try-with-resources statement, which will ensure, that the declared resources are closed
            InputStream is = p.getInputStream();
            InputStreamReader isr = new InputStreamReader(is);
            BufferedReader br = new BufferedReader(isr)
        ) {
            final TimeoutThread tt = new TimeoutThread(p);
            final Thread t = new Thread(tt);
            t.start();
            // Search for result
            while (!tt.hasBeenAborted()) {
                // Only try to read input, if there is something to read. This
                // ensures that this loop can react to the test being aborted by
                // a timeout.
                // Since this basically amounts to busy waiting (although there is
                // a sleep instruction) I am not sure, whether it is the best way
                // to do it. However, for now it seems to work.
                if (br.ready()) { 
                    final String line = br.readLine();
                    if (line == null)
                        break;
                    if (line.startsWith("RES=")) // see `genCode' above
                        val = line.split("=")[1];
                        break; // there is no point to continue the loop, if the result has been retrieved.
                }

                Thread.sleep(1000); // Wait 1 second before trying again
            }
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
    private boolean aborted = false;

    public TimeoutThread(Process p) {
        super();
        this.p = p;
    }

    @Override
    public void run() {
        try {
            Thread.sleep(10000); // 10 second timeout before aborting the test (which for example can happen in the case of a deadlock test)

            if (p.isAlive()) { // If the test is still running by now, terminate it
                p.destroyForcibly();
                hasBeenAborted = true; // record, that the test has not stopped by itself.
            }
        } catch (InterruptedException e) {
        } catch (IOException e) {
        } 
    }

    public boolean hasBeenBorted() {
        return this.aborted;
    }
}
