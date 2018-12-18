/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.frontend.typesystem;

import static org.abs_models.ABSTest.Config.TYPE_CHECK;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import static org.junit.Assume.assumeTrue;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import org.abs_models.backend.common.InternalBackendException;
import org.abs_models.common.WrongProgramArgumentException;
import org.abs_models.frontend.FrontendTest;
import org.abs_models.frontend.analyser.SemanticCondition;
import org.abs_models.frontend.analyser.SemanticConditionList;
import org.abs_models.frontend.ast.Model;
import org.abs_models.frontend.parser.Main;
import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

@RunWith(Parameterized.class)
public class CaseStudyTypeChecking extends FrontendTest {

    /**
     * Use a property to be able to point JUnit in the right direction and override the default
     */
    private static String EXAMPLES_DIR = System.getProperty("abs.junit.examples", "../examples/");
    private static String ENVISAGE_DIR = System.getProperty("abs.junit.envisage", "~/envisage/");

    @Parameters(name="{0}")
    public static Collection<?> data() {
	final Object[][] data = new Object[][] {
	    // https://github.com/abstools/abstools/issues/241
	    // { EXAMPLES_DIR + "T4.3/D4.3.1/FredhopperCloudServices.abs" },
	    { EXAMPLES_DIR + "T4.2/D4.2.1/Indexing.abs" },
	    { EXAMPLES_DIR + "T4.2/D4.2.1/Crawling.abs" },
	    { EXAMPLES_DIR + "T4.2/D4.2.1/MapReduce.abs" },
	    { EXAMPLES_DIR + "T4.2/D4.2.1/Downloading.abs" },
	    { EXAMPLES_DIR + "T4.4/D4.4.1/ETICS.abs" },
	    { ENVISAGE_DIR + "WP4/T4.2/integrated/Atbrox.abs" },
	    { ENVISAGE_DIR + "WP4/T4.2/integrated/AtbroxMultiHandset.abs" },
	    { ENVISAGE_DIR + "WP4/T4.3/model/FredhopperCloudServices_NoDeltas.abs" },
	    { ENVISAGE_DIR + "WP4/T4.3/model/FredhopperCloudServices_NoDeltas_v2.abs" },
	    { ENVISAGE_DIR + "WP4/T4.3/model/FredhopperCloudServices_NoDeltas_v2_CloudProvider.abs" }
	};
	return Arrays.asList(data);
    }

    final private String input;
    protected Model m;

    public CaseStudyTypeChecking(String input) {
        this.input = input;
    }

    @Test
    public void test() throws Exception {
        File srcFolderF = new File(input);
        assumeTrue(srcFolderF.exists());
        m = assertParseFilesOk(input, TYPE_CHECK);
    }

    protected Model assertParseFilesOk(String srcFolder, Config... config) throws IOException,
        WrongProgramArgumentException, InternalBackendException {
        File srcFolderF = new File(srcFolder);
        assertTrue(srcFolder,srcFolderF.exists());
        Main main = new Main();
        Model m = main.parseFiles(false, findAbsFiles(srcFolderF).toArray(new String[0]));

        if (m != null) {
            m.evaluateAllProductDeclarations();
            if (m.hasParserErrors())
                Assert.fail(m.getParserErrors().get(0).getMessage());
            int numSemErrs = m.getErrors().getErrorCount();
            StringBuffer errs = new StringBuffer("Semantic errors: " + numSemErrs + "\n");
            if (numSemErrs > 0) {
                for (SemanticCondition error : m.getErrors())
                    errs = errs.append(error.getHelpMessage() + "\n");
                fail("Failed to parse: " + srcFolder + "\n" + errs.toString());
            } else if (isSet(TYPE_CHECK, config)) {
                SemanticConditionList l = m.typeCheck();
                if (l.containsErrors()) {
                    for (SemanticCondition error : l)
                        errs = errs.append(error.getHelpMessage() + "\n");
                    fail("Failed to typecheck: " + srcFolder + "\n" + errs.toString());
                }
            }
        }
        return m;
    }

    private List<String> findAbsFiles(File srcFolder) {
        List<String> result = new java.util.LinkedList<>();
        if (srcFolder.exists() && !srcFolder.isDirectory()) {
            assertTrue(srcFolder.getName().endsWith(".abs"));
            result.add(srcFolder.getAbsolutePath());
        } else {
            for (File f : srcFolder.listFiles()) {
                if (f.isDirectory()) {
                    result.addAll(findAbsFiles(f));
                } else if (f.getName().endsWith(".abs")) {
                    result.add(f.getAbsolutePath());
                }
            }
        }
        return result;
    }
}
