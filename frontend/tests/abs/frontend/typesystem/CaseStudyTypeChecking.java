/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.typesystem;

import static abs.ABSTest.Config.TYPE_CHECK;
import static abs.ABSTest.Config.WITH_STD_LIB;
import static org.junit.Assert.*;
import static org.junit.Assume.*;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

import abs.common.WrongProgramArgumentException;
import abs.frontend.FrontendTest;
import abs.frontend.analyser.SemanticCondition;
import abs.frontend.analyser.SemanticConditionList;
import abs.frontend.ast.Model;
import abs.frontend.parser.Main;

@RunWith(Parameterized.class)
public class CaseStudyTypeChecking extends FrontendTest {

    /**
     * Use a property to be able to point JUnit in the right direction and override the default
     */
    private static String EXAMPLES_DIR = System.getProperty("abs.junit.examples", "../examples/");
    private static String ENVISAGE_DIR = System.getProperty("abs.junit.envisage", "~/envisage/");

    @Parameters(name="{0}")
    public static Collection<?> data() {
        final Object[][] data = new Object[][] { { EXAMPLES_DIR + "T4.3/D4.3.1/FredhopperCloudServices.abs" }
                                               , { EXAMPLES_DIR + "T4.2/D4.2.1/Indexing.abs" }
                                               , { EXAMPLES_DIR + "T4.2/D4.2.1/Crawling.abs" }
                                               , { EXAMPLES_DIR + "T4.2/D4.2.1/MapReduce.abs" }
                                               , { EXAMPLES_DIR + "T4.2/D4.2.1/Downloading.abs" }
                                               , { EXAMPLES_DIR + "T4.4/D4.4.1/" }
                                               , { ENVISAGE_DIR + "WP4/T4.2/integrated/Atbrox.abs" }
                                               , { ENVISAGE_DIR + "WP4/T4.2/integrated/AtbroxMultiHandset.abs" }
                                               , { ENVISAGE_DIR + "WP4/T4.3/model/FredhopperCloudServices_NoDeltas.abs" }
                                               , { ENVISAGE_DIR + "WP4/T4.3/model/FredhopperCloudServices_NoDeltas_v2.abs" }
                                               , { ENVISAGE_DIR + "WP4/T4.3/model/FredhopperCloudServices_NoDeltas_v2_CloudProvider.abs" }
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
        m = assertParseFilesOk(input, TYPE_CHECK, WITH_STD_LIB);
    }

    protected Model assertParseFilesOk(String srcFolder, Config... config) throws IOException, WrongProgramArgumentException {
        File srcFolderF = new File(srcFolder);
        assertTrue(srcFolder,srcFolderF.exists());
        Main main = new Main();
        main.setWithStdLib(isSet(WITH_STD_LIB,config));
        Model m = main.parseFiles(findAbsFiles(srcFolderF).toArray(new String[0]));

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
        List<String> result = new java.util.LinkedList<String>();
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
