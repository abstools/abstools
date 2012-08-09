/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.common;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;

import abs.backend.BackendTestDriver;

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
    public void assignStmt() {
        assertEvalTrue("{ Bool testresult = False; Bool x = True; testresult = x; }");
    }

    @Test
    public void useOfVariablesInsideExpr() {
        assertEvalTrue("{ Bool testresult = False; Bool b = True; testresult = case b { _ => b; }; }");
    }

}
