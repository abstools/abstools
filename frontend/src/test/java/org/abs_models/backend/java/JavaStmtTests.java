/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java;

import org.junit.Test;

public class JavaStmtTests extends JavaBackendTest {

    @Test
    public void emptyBlock() throws Exception {
        assertValid("{ }");
    }

    @Test
    public void skipStmt() throws Exception {
        assertValid("{ skip; }");
    }
    
    @Test
    public void interfaceNamedLikeModule() throws Exception {
        assertValid("interface JavaUnitTest { JavaUnitTest m(); } { JavaUnitTest i; i.m();}");
    }

    @Test
    public void assertStmt() throws Exception {
        assertValid("{ assert True; }");
    }

    @Test
    public void ifStmt() throws Exception {
        assertValid("{ if (True) { } }");
    }

    @Test
    public void ifElseStmt() throws Exception {
        assertValid(" { if (True) { } else { } }");
    }

    @Test
    public void ifConditionStmt() throws Exception {
        assertValid("{ if (5 == 6) { } }");
    }

    @Test
    public void whileStmt() throws Exception {
        assertValid(" { Bool x = True; while (x) { } }");
    }

    @Test
    public void suspendStmt() throws Exception {
        assertValid(" { suspend; }");
    }

    @Test
    public void awaitStmtField() throws Exception {
        assertValid(" class C { Bool b = True; Unit m() { await this.b; } } ");
    }
    
    @Test
    public void awaitStmtFutGuard() throws Exception {
        assertValid(" { Fut<Bool> f; await f?; }");
    }

    @Test
    public void awaitStmtAndGuard() throws Exception {
        assertValid(" { Fut<Bool> f; await f? & f?; }");
    }

    @Test
    public void awaitStmtExpGuardConstr() throws Exception {
        assertValid(" { await True; }");
    }

    @Test
    public void awaitStmtExpGuardField() throws Exception {
        assertValid("class C { Bool f = True; Unit m() { await f; } }");
    }

    @Test
    public void awaitStmtExpGuardParameter() throws Exception {
        assertValid("class C { Int counter = 0; Unit m(Int i) { await counter > i; } }");
    }
 

    @Test
    public void awaitStmtExpGuardParameter_funcApp() throws Exception {
        // see #381
        assertValid("def Bool greater(Int a, Int b) = a > b; class C { Int counter = 0; Unit m(Int i) { await greater(counter, i); } }");
    }
    
    @Test
    public void awaitStmtExpGuardParameter_letExpr() throws Exception {
        assertValid("class C { Int counter = 0; Unit m(Int i) { await let (Int x) = counter in x > i; } }");
    }
    
    @Test
    public void awaitStmtExpGuardParameter_ifExpr() throws Exception {
        assertValid("class C { Int counter = 0; Unit m(Int i) { await when counter > i then counter > i else False; } }");
    }
    
    @Test
    public void awaitStmtExpGuardLocalVar() throws Exception {
        assertValid("class C { Int counter = 0; Unit m() { Int i = 3; i = i + 1; await counter > i; i = i + 1; } }");
    }
    

    @Test
    public void awaitStmtExpGuardAndLocalVar() throws Exception {
        assertValid("class C { Int counter = 0; Unit m() { Int i = 3; await counter > i & counter > 50; } }");
    }
    
    
    @Test
    public void methodCall() throws Exception {
        assertValid("interface I { Unit m(); } { I i; i.m(); }");
    }

    @Test
    public void methodCallWithArgs() throws Exception {
        assertValid("interface I { Unit m(Bool b, Int i); } { I i; i.m(True, 5); }");
    }

    @Test
    public void typeSynonyms() throws Exception {
        assertValid("type X = I; interface I { } { I i; X x; i = x; x = i; }");
    }

    @Test
    public void typeSynonyms2() throws Exception {
        assertValid("type Data = Int; { Int i = 5; Data d = 6; i = d; d = i; }");
    }

    @Test
    public void typeSynonyms3() throws Exception {
        assertValid("type Data = Int; data DataList = DataNil | ConsData(Data, DataList); "
                + "{ DataList l = ConsData(5,DataNil); Bool testresult = case l { ConsData(x,y) => x == 5;}; }");
    }

    @Test
    public void endlessWhileLoopMethod() throws Exception {
        // Check that we don't trigger Java's simpleton unreachable
        // code warning
        assertValid("""
            class C {
                Unit m() {
                    while (True) {}
                }
            }
            """);
    }

}
