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
        assertValidStdLib("{ assert True; }");
    }

    @Test
    public void ifStmt() throws Exception {
        assertValidStdLib("{ if (True) { } }");
    }

    @Test
    public void ifElseStmt() throws Exception {
        assertValidStdLib(" { if (True) { } else { } }");
    }

    @Test
    public void ifConditionStmt() throws Exception {
        assertValidStdLib("{ if (5 == 6) { } }");
    }

    @Test
    public void whileStmt() throws Exception {
        assertValidStdLib(" { while (True) { } }");
    }

    @Test
    public void suspendStmt() throws Exception {
        assertValidStdLib(" { suspend; }");
    }

    @Test
    public void awaitStmtField() throws Exception {
        assertValidStdLib(" class C { Bool b = True; Unit m() { await this.b; } } "); 
    }
    
    @Test
    public void awaitStmtFutGuard() throws Exception {
        assertValidStdLib(" { Fut<Bool> f; await f?; }");
    }

    @Test
    public void awaitStmtAndGuard() throws Exception {
        assertValidStdLib(" { Fut<Bool> f; await f? & f?; }");
    }

    @Test
    public void awaitStmtExpGuardConstr() throws Exception {
        assertValidStdLib(" { await True; }");
    }

    @Test
    public void awaitStmtExpGuardField() throws Exception {
        assertValidStdLib("class C { Bool f = True; Unit m() { await f; } }");
    }

    @Test
    public void awaitStmtExpGuardParameter() throws Exception {
        assertValidStdLib("class C { Int counter = 0; Unit m(Int i) { await counter > i; } }");
    }
 

    @Test
    public void awaitStmtExpGuardParameter_funcApp() throws Exception {
        // see #381
        assertValidStdLib("def Bool greater(Int a, Int b) = a > b; class C { Int counter = 0; Unit m(Int i) { await greater(counter, i); } }");
    }
    
    @Test
    public void awaitStmtExpGuardParameter_letExpr() throws Exception {
        assertValidStdLib("class C { Int counter = 0; Unit m(Int i) { await let (Int x) = counter in x > i; } }");
    }
    
    @Test
    public void awaitStmtExpGuardParameter_ifExpr() throws Exception {
        assertValidStdLib("class C { Int counter = 0; Unit m(Int i) { await if counter > i then counter > i else False; } }");
    }
    
    @Test
    public void awaitStmtExpGuardLocalVar() throws Exception {
        assertValidStdLib("class C { Int counter = 0; Unit m() { Int i = 3; i = i + 1; await counter > i; i = i + 1; } }");
    }
    

    @Test
    public void awaitStmtExpGuardAndLocalVar() throws Exception {
        assertValidStdLib("class C { Int counter = 0; Unit m() { Int i = 3; await counter > i & counter > 50; } }");
    }
    
    
    @Test
    public void methodCall() throws Exception {
        assertValidStdLib("interface I { Unit m(); } { I i; i.m(); }");
    }

    @Test
    public void methodCallWithArgs() throws Exception {
        assertValidStdLib("interface I { Unit m(Bool b, Int i); } { I i; i.m(True, 5); }");
    }

    @Test
    public void typeSynonyms() throws Exception {
        assertValidStdLib("type X = I; interface I { } { I i; X x; i = x; x = i; }");
    }

    @Test
    public void typeSynonyms2() throws Exception {
        assertValidStdLib("type Data = Int; { Int i = 5; Data d = 6; i = d; d = i; }");
    }

    @Test
    public void typeSynonyms3() throws Exception {
        assertValidStdLib("type Data = Int; data DataList = DataNil | ConsData(Data, DataList); "
                + "{ DataList l = ConsData(5,DataNil); Bool testresult = case l { ConsData(x,y) => x == 5;}; }");
    }

}
