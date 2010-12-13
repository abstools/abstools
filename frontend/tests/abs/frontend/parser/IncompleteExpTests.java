package abs.frontend.parser;

import org.junit.Test;

import abs.frontend.FrontendTest;

public class IncompleteExpTests extends FrontendTest {
    
    @Test
    public void incompleteAccess() {
        assertParseOk("{ x. }"); 
    }
    
}
