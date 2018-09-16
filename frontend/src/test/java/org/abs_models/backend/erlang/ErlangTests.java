/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.erlang;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;

import java.io.File;

import org.abs_models.ABSTest;
import org.abs_models.frontend.ast.Model;

public class ErlangTests extends ABSTest {

    ErlangTestDriver driver = new ErlangTestDriver();

    public void assertEvalTrue(File f) {
        Model m;
        try {
            m = ABSTest.assertParseFileOk(f.getPath(), Config.WITH_STD_LIB);
            assertNotNull(m.lookupModule("BackendTest"));
            assertFalse(m.hasParserErrors());
            assertFalse(m.hasTypeErrors());
            assertNotNull(m.lookupModule("BackendTest"));
            driver.assertEvalTrue(m);
        } catch (RuntimeException e) {
            throw e;
        } catch (Exception e) {
            throw new RuntimeException(e); // TODO: remove; too many too handle
                                           // for now.
        }
    }

    // No Erlang-specific tests at the moment

    // If you add tests here, re-activate this class in AllErlangTests.java or they won't run
}
