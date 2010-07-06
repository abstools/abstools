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
        assertValid("data Bool { True; False; } { if (True) { } }");
    }
   
    @Test
    public void testIfElseStmt() {
        assertValid("data Bool { True; False; } { if (True) { } else { } }");
    }
    
    @Test
    public void testIfConditionStmt() {
        assertValid("{ if (5 == 6) { } }");
    }

    @Test
    public void testWhileStmt() {
        assertValid("data Bool { True; False; } { while (True) { } }");
    }

}
