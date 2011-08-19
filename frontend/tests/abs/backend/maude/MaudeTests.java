/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.maude;

import java.io.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import junit.framework.Assert;

import org.junit.Test;

import abs.ABSTest;
import abs.frontend.ast.Model;
import abs.frontend.parser.Main;

public class MaudeTests extends ABSTest {

    @Test
    public void simpleBlock() {
        assertTrueMaude("module Test; { Bool testresult = True;}");
    }

    public void assertTrueMaude(String absCode) {
        assertMaudeResult(absCode, "True");
    }

    public void assertFalseMaude(String absCode) {
        assertMaudeResult(absCode, "False");
    }

    void assertMaudeResult(String absCode, String expectedResult) {
        try {
            // TODO: test with other simulators (equational, timed) here as well
            String generatedMaudeCode = getMaudeCode(absCode, MaudeCompiler.SIMULATOR_RL);
            String maudeOutput = getMaudeOutput(generatedMaudeCode);
            Pattern pattern = Pattern.compile(".*'testresult \\|-> \"(\\w+)\"\\[emp\\].*");
            Matcher matcher = pattern.matcher(maudeOutput);
            if (matcher.find()) {
                String boolValue = matcher.group(1);
                Assert.assertEquals(expectedResult, boolValue);
            } else {
                System.out.println(maudeOutput);
                Assert.fail("Did not find Maude \"testresult\" variable.");
            }
        } catch (Exception e) {
            e.printStackTrace();
            Assert.fail(e.getMessage());
        }
    }

    protected String getMaudeCode(String absCode, String module) {
        try {
            Model model = null;
            try {
                model = Main.parseString(absCode, true);
            } catch (Exception e) {
                Assert.fail(e.getMessage());
                return null;
            }

            if (model.hasErrors()) {
                Assert.fail(model.getErrors().getFirst().getHelpMessage());
                return null;
            }
            ByteArrayOutputStream out = new ByteArrayOutputStream();
            // TODO: handle delta generation / testing / flattening
            model.generateMaude(new PrintStream(out), module);
            String res = out.toString();
            return res;
        } catch (NumberFormatException e) {
            Assert.fail(e.getMessage());
            return null;
        }
    }

    protected String getMaudeOutput(String maudeCode) throws IOException, InterruptedException {
        StringBuffer result = new StringBuffer();
        // Assuming `maude' is in $PATH here.

        ProcessBuilder pb = new ProcessBuilder();

        File interpreterDir = new File("src"+File.separator+"abs"+File.separator+"backend"+File.separator+"maude");
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
        Process p = pb.start();

        BufferedReader in = new BufferedReader(new InputStreamReader(p.getInputStream()));
        PrintWriter out = new PrintWriter(p.getOutputStream());
        while (in.ready()) {
            result.append(in.readLine());
        }
        out.println(maudeCode);
        out.flush();
        while (in.ready()) {
            result.append(in.readLine());
        }
        out.println("rew start .");
        out.flush();
        while (in.ready()) {
            result.append(in.readLine() + "\n");
        }
        out.println("quit");
        out.flush();
        p.waitFor();
        while (in.ready()) {
            result.append(in.readLine() + "\n");
        }
        return result.toString();
    }

    public void assertFails(String absCode) {
        // TODO: implement
        Assert.fail("Not implemented in Maude Test Backend yet");
    }
}
