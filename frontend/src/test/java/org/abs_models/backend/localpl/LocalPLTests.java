/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.localpl;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.abs_models.ABSTest.Config.USES_LOCAL_PL;

import org.abs_models.ABSTest;
import org.abs_models.frontend.ast.ASTNode;
import org.abs_models.frontend.ast.DeltaDecl;
import org.abs_models.frontend.ast.Model;
import org.junit.Test;

public class LocalPLTests extends ABSTest {

    @Test
    public void railsTest() throws Exception {
        String ms = readFile("abssamples/backend/LocalProductTests/Rails.abs");
        Model m = assertParse(ms, USES_LOCAL_PL);
        assertEquals(m.getModuleDecls().size(),13);
    }
    @Test
    public void reportsTest() throws Exception {
        String ms = readFile("abssamples/backend/LocalProductTests/Total.abs");
        Model m = assertParse(ms, USES_LOCAL_PL);
        assertEquals(m.getModuleDecls().size(),13);
    }


}
