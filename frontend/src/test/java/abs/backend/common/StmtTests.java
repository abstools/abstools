/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.common;

import java.io.File;

import org.junit.Assume;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;

import abs.backend.BackendTestDriver;
import abs.backend.BackendTestDriver.BackendName;

@RunWith(Parameterized.class)
public class StmtTests extends SemanticTests {

    public StmtTests(BackendTestDriver d) {
        super(d);
    }

    @Test
    public void ifThen() {
        assertEvalTrue("{ Bool testresult = False; if (True) testresult = True;  }");
    }

    @Test
    public void ifFalse() {
        assertEvalTrue("{ Bool testresult = False; if (False) skip; else testresult = True;  }");
    }

    @Test
    public void whileFalse() {
        assertEvalTrue("{ Bool testresult = False; while (False) { } testresult = True;  }");
    }

    @Test
    public void whileStmt() {
        assertEvalTrue("{ Bool testresult = False; List<Bool> l = list[False, True]; while (!isEmpty(l)) { l = tail(l); } testresult = True;  }");
    }

    @Test
    public void assignStmt() {
        assertEvalTrue("{ Bool testresult = False; Bool x = True; testresult = x; }");
    }
    
    @Test
    public void declIfExpStmt() {
        // dynamic Java backend generated invalid code for this case
        assertEvalTrue("{Bool x = True; Int i = if x then 1 else 2; Bool testresult = (i == 1); }");
    }

    @Test
    public void assignIfExpStmt() {
        // dynamic Java backend generated invalid code for this case
        assertEvalTrue("{Bool x = True; Int i = 0; i = if x then 1 else 2; Bool testresult = (i == 1); }");
    }

    @Test
    public void useOfVariablesInsideExpr() {
        assertEvalTrue("{ Bool testresult = False; Bool b = True; testresult = case b { _ => b; }; }");
    }

    @Test
    public void caseStatementTest() {
        assertEvalTrue("{ Int x = 0; case Pair(2, 3) { Pair(2, y) => x = y; _ => skip; } Bool testresult = x == 3; }");
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
    public void caseStmtEmbeddedVarDecl() {
        // bug 164
        assertEvalTrue("{ case Just(5) { Just(y) => { Rat z = abs(y); } } Bool testresult = True; }");
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
    public void foreachEmpty() throws Exception {
        assertEvalTrue(new File("abssamples/backend/StmtTests/foreach-empty.abs"));
     }


}
