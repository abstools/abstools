package abs.backend.java;

import org.junit.Test;

public class StmtTests extends JavaBackendTest {
    
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

}
