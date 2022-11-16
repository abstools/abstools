/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.tests;

import org.abs_models.backend.erlang.ErlApp;
import org.abs_models.backend.erlang.ErlangBackend;
import org.abs_models.frontend.typesystem.CaseStudyTypeChecking;
import org.junit.Test;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.PrintStream;
import java.util.EnumSet;

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
    public void testProlog() throws Exception {
        super.test();
        File outFile = File.createTempFile("absjunit-pl", null);
        outFile.deleteOnExit();
        m.generateProlog(new PrintStream(new BufferedOutputStream(new FileOutputStream(outFile))), null);
    }

    @Test
    public void testErlang() throws Exception {
        super.test();
        File tmp = java.nio.file.Files.createTempDirectory("tmp_erl").toFile();
        tmp.deleteOnExit();
        ErlApp ea = new ErlApp(tmp, null, null);
        m.generateErlangCode(ea, EnumSet.noneOf(ErlangBackend.CompileOptions.class));
        ea.close();
    }
}
