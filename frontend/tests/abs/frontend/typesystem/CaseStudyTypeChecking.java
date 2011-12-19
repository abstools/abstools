/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.typesystem;

import static abs.ABSTest.Config.TYPE_CHECK;
import static abs.ABSTest.Config.WITH_STD_LIB;
import static org.junit.Assert.fail;

import java.io.File;
import java.io.IOException;
import java.util.List;

import org.junit.Test;

import choco.cp.solver.constraints.global.geost.dataStructures.LinkedList;

import abs.ABSTest.Config;
import abs.frontend.FrontendTest;
import abs.frontend.analyser.SemanticError;
import abs.frontend.analyser.SemanticErrorList;
import abs.frontend.ast.Model;
import abs.frontend.parser.Main;

public class CaseStudyTypeChecking extends FrontendTest {

    // relative path to https://repos.hats-project.eu:444/svn/hats/CaseStudies/models
    private static final String CASESTUDY_DIR = "../../../../CaseStudies/models/"; 
    

    @Test
    public void testFredhopper() throws Exception {
        assertTypeCheckDirOk("fredhopper/replication/abs");
    }
    
    @Test
    public void testFredhopper2() throws Exception {
        assertTypeCheckDirOk("fredhopper/replication/abs-single/annual-meeting-2011");
    }
    
    @Test
    public void testFredhopper3() throws Exception {
        assertTypeCheckDirOk("fredhopper/replication/abs-single/annual-meeting-2011-async");
    }
    

    @Test
    public void testTradingsystem() throws Exception {
        assertTypeCheckDirOk("tradingsystem");
    }
    
    @Test
    public void testVof() throws Exception {
        assertTypeCheckDirOk("vof");
    }
    
    
    private void assertTypeCheckDirOk(String srcfolder) throws Exception {
        assertParseFilesOk(CASESTUDY_DIR+srcfolder, TYPE_CHECK, WITH_STD_LIB);
    }
    
    protected Model assertParseFilesOk(String srcFolder, Config... config) throws IOException {
        Model m = null;
        Main main = new Main();
        main.setWithStdLib(isSet(WITH_STD_LIB,config));
        File srcFolderF = new File(srcFolder);
        if (!srcFolderF.exists()) {
            fail("Folder " + srcFolder + " || " + srcFolderF.getAbsolutePath() + " does not exist.");
        }
        m = main.parseFiles(findAbsFiles(srcFolderF).toArray(new String[0]));
        
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
