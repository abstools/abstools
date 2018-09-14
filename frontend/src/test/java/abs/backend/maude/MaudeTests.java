/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.maude;

import java.io.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.junit.Assert;
import org.junit.Assume;
import org.junit.BeforeClass;
import org.junit.Test;

import abs.ABSTest;
import abs.frontend.ast.Model;

/**
 * Note that this class doesn't have a nullary constructor and can't be run
 * as a JUnit test itself!
 */
public class MaudeTests extends ABSTest {

    final MaudeCompiler.SIMULATOR mode;

    public MaudeTests(MaudeCompiler.SIMULATOR mode) {
        this.mode = mode;
    }

    /* These two are direct entry-points for JUnit */
    public static class MaudeEqTests extends MaudeTests {
        public MaudeEqTests() {
            super(MaudeCompiler.SIMULATOR.EQ_TIMED);
        }
    }

    public static class MaudeRlTests extends MaudeTests {
        public MaudeRlTests() {
            super(MaudeCompiler.SIMULATOR.RL);
        }
    }

    @BeforeClass
    public static void checkMaude() {
        ProcessBuilder pb = new ProcessBuilder();
        pb.command("maude");
        try {
            Process p = pb.start();
            p.destroy();
        } catch (IOException e) {
            Assume.assumeNoException(e);
        }
    }

    @Test
    public void simpeBlock() throws Exception {
        assertTrueMaude("module Test; { Bool testresult = True;}");
    }

    public void assertTrueMaude(String absCode) throws Exception {
        assertMaudeResult(absCode, "ABS.StdLib.True");
    }

    public void assertFalseMaude(String absCode) throws Exception {
        assertMaudeResult(absCode, "ABS.StdLib.False");
    }

    void assertMaudeResult(String absCode, String expectedResult) throws Exception {
        String generatedMaudeCode = getMaudeCode(absCode, mode);
        assertMaudeCodeResult(generatedMaudeCode, expectedResult);
    }

    final static Pattern patternResult = Pattern.compile(".*'testresult \\|-> \"(.*?)\"\\[emp\\].*");

    public void assertMaudeCodeResult(String generatedMaudeCode, String expectedResult) throws Exception {
        String maudeOutput = getMaudeOutput(generatedMaudeCode);
        Matcher matcher = patternResult.matcher(maudeOutput);
        if (matcher.find()) {
            String boolValue = matcher.group(1);
            Assert.assertEquals(expectedResult, boolValue);
        } else {
            System.err.println(maudeOutput);
            Assert.fail("Did not find Maude \"testresult\" variable.");
        }
    }

    protected String getMaudeCode(String absCode, MaudeCompiler.SIMULATOR module) throws Exception {
        Model model = assertParseOk(absCode, Config.WITH_STD_LIB, Config.WITHOUT_MODULE_NAME, Config.TYPE_CHECK /* XXX:CI, Config.WITH_LOC_INF */);
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        // TODO: handle delta generation / testing / flattening
        model.generateMaude(new PrintStream(out), module, 100, 0);
        String res = out.toString();
        return res;
    }

    protected String getMaudeOutput(String maudeCode) throws IOException, InterruptedException {
        final StringBuffer result = new StringBuffer();
        // Assuming `maude' is in $PATH here.

        ProcessBuilder pb = new ProcessBuilder();
        final String s = File.separator;
        File interpreterDir = new File("src"+s+"main"+s+"resources"+s+"maude");
        //File interpreterDir = new File(new File(System.getProperty("user.dir")).getParentFile(), "interpreter");
        
        File interpreter = new File(interpreterDir, "abs-interpreter.maude");
        String[] cmd = { "maude", "-no-banner", "-no-ansi-color", "-no-wrap", "-batch", interpreter.getAbsolutePath() };
        pb.command(cmd);
        pb.directory(interpreterDir);

        if (!interpreter.exists()) {
            Assert.fail(interpreter.getAbsolutePath() + " does not exist!");
        }
        // pb.directory(workingDir);
        pb.redirectErrorStream(true);
        final Process p = pb.start();

        final BufferedReader in = new BufferedReader(new InputStreamReader(p.getInputStream()));
        PrintWriter out = new PrintWriter(p.getOutputStream());
        Thread pin = new Thread(new Runnable() {
            @Override
            public void run() {
                try {
                    String l;
                    while ((l = in.readLine()) != null) {
                        result.append(l + "\n");
                    }
                } catch (IOException e) {}
            }            
        });
        pin.start();
        out.println(maudeCode);
        // Shouldn't block here
        out.println("rew start .\nquit");
        out.flush();
        Runnable killer = new Runnable() {
            
            @Override
            public void run() {
                try {
                    Thread.sleep(1000*5);
                    System.err.println("Timeout!");
                    p.destroy();
                } catch (InterruptedException e) {}
            }
        };
        Thread kT = new Thread(killer);
        kT.start();
        p.waitFor();
        kT.interrupt();
        pin.join();
        out.close();
        in.close();
        return result.toString();
    }

    final static Pattern patternFailure = Pattern.compile("\\| failure");
    public void assertFails(String absCode) throws Exception {
        String generatedMaudeCode = getMaudeCode(absCode, mode);
        String maudeOutput = getMaudeOutput(generatedMaudeCode);
        Matcher matcher = patternFailure.matcher(maudeOutput);
        if (!matcher.find()) {
            Assert.fail("Did not find ANY indication of failure...");
        }
    }
}
