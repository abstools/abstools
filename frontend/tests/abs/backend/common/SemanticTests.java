/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.common;

import static org.junit.Assert.assertNotNull;

import java.io.IOException;
import java.util.Collection;
import java.util.LinkedList;

import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

import abs.backend.BackendTestDriver;
import abs.backend.abs2haskell.ABS2HaskellDriver;
import abs.frontend.ast.Model;

@RunWith(Parameterized.class)
public abstract class SemanticTests {
    final protected BackendTestDriver driver;

    public SemanticTests(BackendTestDriver d) {
        driver = d;
    }

    public static boolean checkMaude() {
        return checkProg("maude");
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
        /* XXX Trimmed for vs-haskell branch, DO NOT MERGE! */
        final Collection<Object[]> data = new LinkedList<Object[]>();
        if (ABS2HaskellDriver.checkA2HS()) {
            data.add(new Object[] { new ABS2HaskellDriver(4) });
        }
        return data;
    }

    public void assertEvalTrue(String absCode) {
        try {
            driver.assertEvalTrue("module BackendTest; " + absCode);
        } catch (RuntimeException e) {
            throw e;
        } catch (Exception e) {
            throw new RuntimeException(e); // TODO: remove; too many too handle
                                           // for now.
        }
    }
    
    public void assertEvalTrue(Model m) {
        try {
            assertNotNull(m.lookupModule("BackendTest"));
            driver.assertEvalTrue(m);
        } catch (RuntimeException e) {
            throw e;
        } catch (Exception e) {
            throw new RuntimeException(e); // TODO: remove; too many too handle
                                           // for now.
        }
    }

    public void assertEvalFails(String absCode) throws Exception {
        driver.assertEvalFails("module BackendTest; " + absCode);
    }
}
