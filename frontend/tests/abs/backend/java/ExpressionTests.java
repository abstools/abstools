package abs.backend.java;

import org.junit.Test;

public class ExpressionTests extends JavaBackendTest {
    
    @Test
    public void testIfExp() {
        assertValid("{ if (True) { } }");
    }
   
    @Test
    public void testIfElseExp() {
        assertValid("{ if (True) { } else { } }");
    }
    
    @Test
    public void testIfConditionExp() {
        assertValid("{ if (5 == 6) { } }");
    }

}
