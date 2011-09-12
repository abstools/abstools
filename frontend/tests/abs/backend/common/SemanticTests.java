/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.common;

import java.io.IOException;
import java.util.Arrays;
import java.util.Collection;

import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

import abs.backend.BackendTestDriver;
import abs.backend.java.JavaTestDriver;
import abs.backend.maude.MaudeCompiler;
import abs.backend.maude.MaudeTestDriver;

@RunWith(Parameterized.class)
public class SemanticTests {
    private BackendTestDriver driver;

    public SemanticTests(BackendTestDriver d) {
        driver = d;
    }

    public static boolean checkMaude() {
        ProcessBuilder pb = new ProcessBuilder();
        pb.command("maude");
        try {
            Process p = pb.start();
            p.destroy();
            return true;
        } catch (IOException e) {
            return false;
        }
    }

    @Parameters
    public static Collection<?> data() {
        final Object[][] data;
        /* TODO: Mark Maude tests as ignored instead of just missing them */
        if (checkMaude())
            data = new Object[][] { { new JavaTestDriver() }, { new JavaTestDriver(1) } , { new MaudeTestDriver(MaudeCompiler.SIMULATOR_RL) } , { new MaudeTestDriver(MaudeCompiler.SIMULATOR_EQ_TIMED) } };
        else
            data = new Object[][] { { new JavaTestDriver() }, { new JavaTestDriver(1) } };
        return Arrays.asList(data);
    }

    public void assertEvalTrue(String absCode) {
        driver.assertEvalTrue("module BackendTest; " + absCode);
    }

    public void assertEvalFails(String absCode) {
        driver.assertEvalFails("module BackendTest; " + absCode);
    }

}
