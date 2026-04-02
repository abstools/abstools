/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.common;

import java.io.File;

import org.junit.Assume;
import org.junit.Ignore;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;

import org.abs_models.backend.BackendTestDriver;
import org.abs_models.backend.BackendTestDriver.BackendName;

@RunWith(Parameterized.class)
public class StmtTests extends SemanticTests {

    public StmtTests(BackendTestDriver d) {
        super(d);
    }

    @Test
    public void ifThen() throws Exception {
        assertEvalTrue("{ Bool testresult = False; if (True) testresult = True;  }");
    }

    @Test
    public void ifFalse() throws Exception {
        assertEvalTrue("{ Bool testresult = False; if (False) skip; else testresult = True;  }");
    }

    @Test
    public void whileFalse() throws Exception {
        // cannot directly write `while(False)`: Java backend fails
        // with error about unreachable code
        assertEvalTrue("{ Bool testresult = False; while (testresult) { } testresult = True;  }");
    }

    @Test
    public void whileStmt() throws Exception {
        assertEvalTrue("{ Bool testresult = False; List<Bool> l = list[False, True]; while (!isEmpty(l)) { l = tail(l); } testresult = True;  }");
    }

    @Test
    public void assignStmt() throws Exception {
        assertEvalTrue("{ Bool testresult = False; Bool x = True; testresult = x; }");
    }
    
    @Test
    public void declWhenExpStmt() throws Exception {
        assertEvalTrue("{Bool x = True; Int i = when x then 1 else 2; Bool testresult = (i == 1); }");
    }

    @Test
    public void assignWhenExpStmt() throws Exception {
        assertEvalTrue("{Bool x = True; Int i = 0; i = when x then 1 else 2; Bool testresult = (i == 1); }");
    }

    @Test
    public void useOfVariablesInsideExpr() throws Exception {
        assertEvalTrue("{ Bool testresult = False; Bool b = True; testresult = case b { _ => b; }; }");
    }

    @Test
    public void caseStatementTest() throws Exception {
        assertEvalTrue("{ Int x = 0; switch (Pair(2, 3)) { Pair(2, y) => x = y; _ => skip; } Bool testresult = x == 3; }");
    }
    
    @Test
    public void caseStmtField() throws Exception {
        assertEvalTrue(new File("abssamples/backend/StmtTests/caseStmtField.abs"));
     }

    @Test
    public void caseStmtBoundLocal() throws Exception {
        assertEvalTrue(new File("abssamples/backend/StmtTests/caseStmtBoundLocal.abs"));
     }

    @Test
    public void caseStmtBoundParameter() throws Exception {
        assertEvalTrue(new File("abssamples/backend/StmtTests/caseStmtBoundParameter.abs"));
     }

    @Test
    public void caseStmtBoundAssignedParameter() throws Exception {
        assertEvalTrue(new File("abssamples/backend/StmtTests/caseStmtBoundAssignedParameter.abs"));
    }

    @Test
    public void caseStmtEmbeddedVarDecl() throws Exception {
        // bug 164
        assertEvalTrue("{ switch (Just(5)) { Just(y) => { Rat z = abs(y); } } Bool testresult = True; }");
    }

    @Test
    public void caseStmtLastMatch() throws Exception {
        assertEvalTrue("""
            {
                String s = "in";
                Bool testresult = False;
                switch (s) {
                    "out" => println("Got value `out`");
                    _ => println("Got another value");
                }
                testresult = True;
            }""");
    }

    @Test
    public void caseStmtMatchFailure() throws Exception {
        // https://github.com/abstools/abstools/issues/196
        Assume.assumeFalse("Not implemented on Java backend yet", driver.getBackendName() == BackendName.JAVA);
        assertEvalTrue(new File("abssamples/backend/StmtTests/caseStmtMatchFailure.abs"));
     }

    @Test
    public void foreachNonempty() throws Exception {
        assertEvalTrue(new File("abssamples/backend/StmtTests/foreach-nonempty.abs"));
     }

    @Test
    public void foreachIndexedNonempty() throws Exception {
        assertEvalTrue(new File("abssamples/backend/StmtTests/foreach-indexed-nonempty.abs"));
     }

    @Test
    public void foreachEmpty() throws Exception {
        assertEvalTrue(new File("abssamples/backend/StmtTests/foreach-empty.abs"));
     }

    @Test
    public void foreachNested() throws Exception {
        assertEvalTrue(new File("abssamples/backend/StmtTests/foreach-nested.abs"));
     }

}
