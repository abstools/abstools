/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.abs2haskell;

import static org.junit.Assert.*;
import static org.junit.Assume.*;

import java.io.File;
import java.io.PrintWriter;

import abs.ABSTest;
import abs.backend.BackendTestDriver;
import abs.backend.common.SemanticTests;
import abs.backend.prettyprint.DefaultABSFormatter;
import abs.frontend.ast.Model;
import abs.frontend.tests.ABSFormatter;

/**
 * @author stolz
 */
public class ABS2HaskellDriver extends ABSTest implements BackendTestDriver {

    private final int cores;
    /**
     * TODO: Randomize tmpDir per run.
     */
    private static String tmpDir = System.getProperty("java.io.tmpdir");
    /**
     * TODO: These variables make concurrent runs impossible.
     */
    final File tmpDirF = new File(tmpDir);
    final String aOut = "a.out";

    public static boolean checkA2HS() {
        return SemanticTests.checkProg("abs2haskell") && SemanticTests.checkProg("ghc");
    }

    public ABS2HaskellDriver(int cores) {
        super();
        assertTrue(cores > 0);
        this.cores = cores;
    }

    public ABS2HaskellDriver() {
       this(2);
    }

    @Override
    public String toString() {
        return "ABS2Haskell";
    }

    @Override
    public void assertEvalEquals(String absCode, boolean value) throws Exception {
        // TODO Auto-generated method stub
        Model m = assertParseOk(absCode, Config.WITH_STD_LIB, Config.TYPE_CHECK);
        generateAndCompile(m);
        assumeTrue(false);
    }

    @Override
    public void assertEvalFails(String absCode) throws Exception {
        // TODO Auto-generated method stub
        Model m = assertParseOk(absCode, Config.WITH_STD_LIB, Config.TYPE_CHECK);
        generateAndCompile(m);
        assumeTrue(false);
    }

    @Override
    public void assertEvalTrue(String absCode) throws Exception {
        Model m = assertParseOk(absCode, Config.WITH_STD_LIB, Config.TYPE_CHECK);
        assertEvalTrue(m);
    }

    public void generateAndCompile(Model m) throws Exception {
        /* This shouldn't happen: */
        assertFalse("You forgot to select a product (abs2haskell doesn't support them)!", m.hasProductLine());
        tmpDirF.createNewFile();
        assertTrue(tmpDirF.exists());
        tmpDirF.deleteOnExit();
        final File tmp = File.createTempFile("a2h_", ".abs",tmpDirF);
        tmp.deleteOnExit(); // just in case
        try {
            /* Since we come in here with a model, we need to pretty-print it first.
             * TODO: check codepath when we come with an actual string and avoid
             * redundant pretty-printing.
             */
            {
                PrintWriter w = new PrintWriter(tmp);
                ABSFormatter f = new DefaultABSFormatter(w);
                m.doPrettyPrint(w, f);
                w.close();
                {
                    // Sanity check -- does it parse?
                    Model m2 = assertParseFileOk(tmp.getCanonicalPath(), Config.WITH_STD_LIB, Config.TYPE_CHECK);
                    /* This will eventually be fixed -- 6 = StdLib(5) + actual test: */
                    assumeTrue("I can't let you do this (yet), Dave: there's more than a single module in there, found "+Integer.toString(m2.getModuleDecls().size()), m2.getModuleDecls().size() == 6);
                }
            }
            /* Tmp a2hs output written, now feed into translit */
            {
                /* We pass the single file in to look for the main block.
                 * TODO: more complex examples w/ multiple files
                 *   -- or are we covered there since we come through the PrettyPrinter anyway?
                 */
                ProcessBuilder pbA2HS = new ProcessBuilder("abs2haskell", "--main-is="+tmp.getCanonicalPath(), tmp.getCanonicalPath());
                pbA2HS.inheritIO(); // Actually we'd like to set stdin to /dev/null, but...
                pbA2HS.directory(tmpDirF);
                int rc = pbA2HS.start().waitFor();
                // Remove intermediate file
                new File(tmpDirF,tmp.getCanonicalPath().replace(".hs", ".ast")).delete();
                assertEquals("abs2haskell failed;", 0, rc);
            }
            /* This is where hopefully the output ended up: */
            File a2hsout = new File(tmpDirF,tmp.getName().replace(".abs", ".hs"));
            assertTrue(a2hsout.exists());
            a2hsout.deleteOnExit();
            {
                ProcessBuilder pbCompile = new ProcessBuilder("ghc","-o", aOut,"--make", "-threaded", a2hsout.getCanonicalPath());
                pbCompile.directory(tmpDirF);
                pbCompile.inheritIO();
                int rc = pbCompile.start().waitFor();
                assertEquals("ghc failed;", 0, rc);
            }
            final File aOutF = new File(tmpDirF,aOut);
            assertTrue(aOutF.exists());
            aOutF.deleteOnExit();
        } finally {
            tmp.delete();
        }
    }

    @Override
    public void assertEvalTrue(Model m) throws Exception {
        generateAndCompile(m);
        /* Now run the binary.
         * TODO: Timeout. */
        {
            ProcessBuilder pbExe = new ProcessBuilder("./"+aOut,"+RTS","-N"+cores);
            pbExe.directory(tmpDirF);
            pbExe.inheritIO();
            Process p = pbExe.start();
            // XXX: capture IO
            int rc = p.waitFor();
            assertEquals("Binary terminated abnormally;", 0, rc);
        }
        assumeTrue(false); // Look for testresult.
        /* aOut has deleteOnExit() set. */
    }
}
