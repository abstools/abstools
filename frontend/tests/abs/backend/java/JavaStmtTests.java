package abs.backend.java;

import org.junit.Test;

public class JavaStmtTests extends JavaBackendTest {
    
    @Test
    public void emptyBlock() {
        assertValid("{ }");
    }

    @Test
    public void emptyStmt() {
        assertValid("{ ; }");
    }

    @Test
    public void skipStmt() {
        assertValid("{ skip; }");
    }
    
    @Test
    public void assertStmt() {
        assertValidStdLib("{ assert True; }");
    }

    @Test
    public void ifStmt() {
        assertValidStdLib("{ if (True) { } }");
    }
   
    @Test
    public void ifElseStmt() {
        assertValidStdLib(" { if (True) { } else { } }");
    }
    
    @Test
    public void ifConditionStmt() {
        assertValidStdLib("{ if (5 == 6) { } }");
    }

    @Test
    public void whileStmt() {
        assertValidStdLib(" { while (True) { } }");
    }

    @Test
    public void suspendStmt() {
        assertValidStdLib(" { suspend; }");
    }
    
    @Test
    public void awaitStmtFutGuard() {
        assertValidStdLib(" { Fut<Bool> f; await f?; }");
    }
    
    @Test
    public void awaitStmtAndGuard() {
        assertValidStdLib(" { Fut<Bool> f; await f? & f?; }");
    }
    
    @Test
    public void awaitStmtExpGuardConstr() {
        assertValidStdLib(" { await True; }");
    }

    @Test
    public void awaitStmtExpGuardField() {
        assertValidStdLib("class C { Bool f = True; Unit m() { await f; } }");
    }
    
    @Test
    public void methodCall() {
       assertValidStdLib("interface I { Unit m(); } { I i; i.m(); }");
    }

    @Test
    public void methodCallWithArgs() {
       assertValidStdLib("interface I { Unit m(Bool b, Int i); } { I i; i.m(True, 5); }");
    }
    
    @Test
    public void typeSynonyms() {
        assertValidStdLib("type X = I; interface I { } { I i; X x; i = x; x = i; }");
    }

    @Test
    public void typeSynonyms2() {
        assertValidStdLib("type Data = Int; { Int i = 5; Data d = 6; i = d; d = i; }");
    }

    @Test
    public void typeSynonyms3() {
        assertValidStdLib("type Data = Int; data DataList = DataNil | ConsData(Data, DataList); " +
        		"{ DataList l = ConsData(5,DataNil); Bool testresult = case l { ConsData(x,y) => x == 5;}; }");
    }
    
    

}


