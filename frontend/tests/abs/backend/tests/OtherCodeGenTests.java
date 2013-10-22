/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.tests;

import static org.junit.Assert.*;

import java.io.*;

import org.junit.*;

import abs.backend.erlang.ErlApp;
import abs.frontend.typesystem.CaseStudyTypeChecking;

public class OtherCodeGenTests extends CaseStudyTypeChecking {

    /* TODO: use random dir */
    public OtherCodeGenTests(String input) {
        super(input);
    }

    @Test @Override
    /* CoreABS */
    public void test() throws Exception {
        super.test();
        File outFile = File.createTempFile("absjunit-core", null);
        outFile.deleteOnExit();
        m.generateCoreABS(new PrintStream(new BufferedOutputStream(new FileOutputStream(outFile))));
    }

    @Test
    public void testScala() throws Exception {
        super.test();
        File tmp = new File(System.getProperty("java.io.tmpdir"));
        assertTrue(tmp.exists() && tmp.canWrite());
        File outputDir = new File(tmp, "absjunit-scala");
        outputDir.mkdir();
        outputDir.deleteOnExit();
        m.generateScala(outputDir);
    }

    @Test
    public void testProlog() throws Exception {
        super.test();
        File outFile = File.createTempFile("absjunit-pl", null);
        outFile.deleteOnExit();
        m.generateProlog(new PrintStream(new BufferedOutputStream(new FileOutputStream(outFile))), null);
    }

    @Test
    public void testErlang() throws Exception {
        super.test();
        File tmpD = new File(System.getProperty("java.io.tmpdir"));
        assertTrue(tmpD.exists() && tmpD.canWrite());
        File tmp = new File(tmpD,"tmp_erl");
        tmp.mkdir();
        tmp.deleteOnExit();
        ErlApp ea = new ErlApp(tmp);
        m.generateErlangCode(ea);
        ea.close();
    }
}
