package abs.backend.java;

import org.junit.Test;

public class StmtTests extends JavaBackendTest {
    
    @Test
    public void testEmptyBlock() {
        assertValid("{ }");
    }

    @Test
    public void testEmptyStmt() {
        assertValid("{ ; }");
    }

    @Test
    public void testSkipStmt() {
        assertValid("{ skip; }");
    }
    
    @Test
    public void testIfStmt() {
        assertValidStdLib("{ if (True) { } }");
    }
   
    @Test
    public void testIfElseStmt() {
        assertValidStdLib(" { if (True) { } else { } }");
    }
    
    @Test
    public void testIfConditionStmt() {
        assertValid("{ if (5 == 6) { } }");
    }

    @Test
    public void testWhileStmt() {
        assertValidStdLib(" { while (True) { } }");
    }

}
