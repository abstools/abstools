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

import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

import abs.backend.maude.MaudeTests;
import abs.frontend.FrontendTest;
import abs.frontend.analyser.SemanticError;
import abs.frontend.analyser.SemanticErrorList;
import abs.frontend.ast.Model;
import abs.frontend.parser.Main;

@RunWith(Parameterized.class)
public class CaseStudyTypeChecking extends FrontendTest {

    // default relative path to https://repos.hats-project.eu:444/svn/hats/CaseStudies/models
    /**
     * Use a property to be able to point JUnit in the right direction and override the default:
     */
    private static String CASESTUDY_DIR = System.getProperty("abs.junit.casestudies", "../../../../CaseStudies/models/"); 

    /**
     * Check if the casestudies are available (see {@link MaudeTests}). Again, BeforeClass
     * seems better than a plain assumeTrue(), as the tests will show as ignored, not passed.
     * @author stolz
     */
    @BeforeClass
    public static void checkExists() {
        File srcFolderF = new File(CASESTUDY_DIR);
        assumeTrue(srcFolderF.exists());
        if (!CASESTUDY_DIR.endsWith("/"))
            CASESTUDY_DIR += "/";
    }

    @Parameters
    public static Collection<?> data() {
        final Object[][] data = new Object[][] { { "fredhopper/replication/abs" }
                                               , { "fredhopper/replication/abs-single/annual-meeting-2011" }
                                               , { "fredhopper/replication/abs-single/annual-meeting-2011-async" }
                                               , { "tradingsystem"}
                                               , { "vof"} };
        return Arrays.asList(data);
    }

    final private String input;
    protected Model m;

    public CaseStudyTypeChecking(String input) {
        this.input = input;
    }

    @Test
    public void test() throws Exception {
        m = assertParseFilesOk(CASESTUDY_DIR+input, TYPE_CHECK, WITH_STD_LIB);
    }

    protected Model assertParseFilesOk(String srcFolder, Config... config) throws IOException {
        File srcFolderF = new File(srcFolder);
        assertTrue(srcFolder,srcFolderF.exists());
        Main main = new Main();
        main.setWithStdLib(isSet(WITH_STD_LIB,config));
        Model m = main.parseFiles(findAbsFiles(srcFolderF).toArray(new String[0]));

        if (m != null) {
            int numSemErrs = m.getErrors().size();
            StringBuffer errs = new StringBuffer("Semantic errors: " + numSemErrs + "\n");
            if (numSemErrs > 0) {
                for (SemanticError error : m.getErrors())
                    errs = errs.append(error.getHelpMessage() + "\n");
                fail("Failed to parse: " + srcFolder + "\n" + errs.toString());
            } else if (isSet(TYPE_CHECK, config)) {
                SemanticErrorList l = m.typeCheck();
                if (!l.isEmpty()) {
                    for (SemanticError error : l)
                        errs = errs.append(error.getHelpMessage() + "\n");
                    fail("Failed to typecheck: " + srcFolder + "\n" + errs.toString());
                }
            }
        }
        return m;
    }

    private List<String> findAbsFiles(File srcFolder) {
        List<String> result = new java.util.LinkedList<String>();
        for (File f : srcFolder.listFiles()) {
            if (f.isDirectory()) {
                result.addAll(findAbsFiles(f));
            } else if (f.getName().endsWith(".abs")) {
                result.add(f.getAbsolutePath());
            }
        }
        return result;
    }
}
