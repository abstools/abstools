/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.erlang;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;

import java.io.File;

import org.junit.Test;

import abs.ABSTest;
import abs.ABSTest.Config;
import abs.frontend.ast.Model;

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

    @Test
    public void deadline1() throws Exception {
        assertEvalTrue(new File("tests/abssamples/backend/TimeTests/deadline1.abs"));
    }
    
    @Test
    public void scheduler_priority() {
        assertEvalTrue(new File("tests/abssamples/backend/TimeTests/scheduler_priority.abs"));
    }

    @Test
    public void scheduler_deadline() {
        assertEvalTrue(new File("tests/abssamples/backend/TimeTests/scheduler_deadline.abs"));
    }
}
