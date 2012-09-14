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
import abs.backend.java.dynamic.JavaDynamicTestDriver;
import abs.backend.maude.MaudeCompiler;
import abs.backend.maude.MaudeTestDriver;

@RunWith(Parameterized.class)
public abstract class SemanticTests {
    final private BackendTestDriver driver;

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
        /* TODO: For the Java backend, we just use different RUNTIME options, not code-gen options.
         * So we could actually just compile the code to Java once, and then run it with the different options.
         */
        /* Append new tests to the end, so that we can aggregate relative differences in CI */
        if (checkMaude())
            data = new Object[][] { { new JavaTestDriver() }, { new JavaTestDriver(1) }, { new MaudeTestDriver(MaudeCompiler.SIMULATOR_RL) } , { new MaudeTestDriver(MaudeCompiler.SIMULATOR_EQ_TIMED) }
            , {new JavaDynamicTestDriver()}
            // FIXME: enable after #302 is done, {new JavaTestDriver(){{absArgs.add("-taskScheduler=simplex");}} }
            };
        else
            data = new Object[][] { { new JavaTestDriver() }, { new JavaTestDriver(1) }
            , {new JavaDynamicTestDriver()}
            //, {new JavaTestDriver(){{absArgs.add("-taskScheduler=simple");}} }
            };
        return Arrays.asList(data);
    }

    public void assertEvalTrue(String absCode) {
        try {
            driver.assertEvalTrue("module BackendTest; " + absCode);
        } catch (Exception e) {
            throw new RuntimeException(e); // TODO: remove; too many too handle for now.
        }
    }

    public void assertEvalFails(String absCode) throws Exception {
        driver.assertEvalFails("module BackendTest; " + absCode);
    }
}
