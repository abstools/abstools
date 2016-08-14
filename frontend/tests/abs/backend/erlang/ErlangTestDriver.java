/**

 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.erlang;

import static org.junit.Assert.assertEquals;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.charset.Charset;
import java.util.regex.Matcher;

import org.apache.commons.io.FileUtils;
import org.junit.Assert;
import org.junit.Assume;

import abs.ABSTest;
import abs.backend.BackendTestDriver;
import abs.backend.common.InternalBackendException;
import abs.backend.common.SemanticTests;
import abs.frontend.ast.*;

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
            // http://stackoverflow.com/a/9561398/60462
            ProcessBuilder pb = new ProcessBuilder("erl", "-eval", "erlang:display(erlang:system_info(otp_release)), halt().",  "-noshell");
            try {
                Process p = pb.start();
                InputStreamReader r = new InputStreamReader(p.getInputStream());
                BufferedReader b = new BufferedReader(r);
                Assert.assertEquals(0, p.waitFor());
                String version = b.readLine();
                java.util.regex.Pattern pat = java.util.regex.Pattern.compile("\"(\\d*).*");
                Matcher m = pat.matcher(version);
                Assert.assertTrue("Could not identify Erlang version: "+version, m.matches());
                String v = m.group(1);
                Assume.assumeTrue("Need Erlang R17 or better.",Integer.parseInt(v) >= 17);
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
        Assert.assertEquals("true", runAndCheck(absCode));
    }

    /**
     * Parses, complies, runs given code and returns value of testresult.
     * 
     * @param absCode
     * @return either the Result, or if an execution error occurred null
     */
    private String runAndCheck(String absCode) throws Exception {
        File f = null;
        try {
            f = Files.createTempDir();
            f.deleteOnExit();
            Model model = assertParseOk(absCode, Config.WITH_STD_LIB, Config.TYPE_CHECK, /* XXX:CI Config.WITH_LOC_INF, */ Config.WITHOUT_MODULE_NAME);
            String mainModule = genCode(model, f, true);
            make(f);
            return run(f, mainModule);
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
            genCode(model, f, true);
            make(f);
        } finally {
            try {
                FileUtils.deleteDirectory(f);
            } catch (IOException e) {
                // Ignore Ex, File should be deleted anyway
            }
        }
    }

    /**
     * Generates Erlang code in target directory
     * 
     * To retrieve the testresult value, we inject in the mainblock a return
     * statement, which will then be the result of the execution
     * 
     * @return the Module Name, which contains the Main Block
     * @throws InternalBackendException 
     * 
     */
    private String genCode(Model model, File targetDir, boolean appendResultprinter) throws IOException, InterruptedException, InternalBackendException {
        if (model.hasErrors()) {
            Assert.fail(model.getErrors().getFirstError().getHelpMessage());
        }
        if (model.hasTypeErrors()) {
            Assert.fail(model.getTypeErrors().getFirstError().getHelpMessage());
        }
        MainBlock mb = model.getMainBlock();
        if (mb != null && appendResultprinter) {
            mb.addStmt(new ReturnStmt(new List<Annotation>(),
                                      new VarUse("testresult")));
        }
        ErlangBackend.compile(model, targetDir, true);
        if (mb == null)
            return null;
        else
            return mb.getModuleDecl().getName();

    }

    /**
     * Complies code in workDir
     */
    private void make(File workDir) throws Exception {
        ProcessBuilder pb = new ProcessBuilder("erl", "-pa", "ebin", "-noshell", "-noinput", "-eval",
                "case make:all() of up_to_date -> halt(0); _ -> halt(1) end.");
        pb.directory(workDir);
        pb.inheritIO();
        Process p = pb.start();
        Assert.assertEquals("Compile failed", 0, p.waitFor());
    }

    private static final String RUN_SCRIPT=  
            "#!/usr/bin/env escript\n"+
            "%%! -pa ebin\n"+
            "main(Arg)->"+
            "V=runtime:start(Arg),"+
            "timer:sleep(10),"+
            "io:format(\"RES=~p~n\",[V]).";
     /**
     * Executes mainModule
     * 
     * We replace the run script by a new version, which will write the Result
     * to STDOUT Furthermore to detect faults, we have a Timeout process, which
     * will kill the runtime system after 2 seconds
     */
    private String run(File workDir, String mainModule) throws Exception {
        String val = null;
        File runFile = new File(workDir, "run");
        Files.write(RUN_SCRIPT, runFile, Charset.forName("UTF-8"));
        runFile.setExecutable(true);
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
            if (line.startsWith("RES="))
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
            make(f);
            assertEquals("true",run(f, mainModule));
        } finally {
            try {
                FileUtils.deleteDirectory(f);
            } catch (IOException e) {
                // Ignore Ex, File should be deleted anyway
            }
        }        
    }
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
            Thread.sleep(2000);
            p.destroy();
        } catch (InterruptedException e) {
        }
    }
}
