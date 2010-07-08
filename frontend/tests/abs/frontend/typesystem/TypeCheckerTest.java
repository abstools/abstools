package abs.frontend.typesystem;

import static org.junit.Assert.*;

import org.junit.Test;

import abs.frontend.FrontendTest;
import abs.frontend.analyser.ErrorMessage;
import abs.frontend.analyser.SemanticErrorList;
import abs.frontend.ast.Model;

public class TypeCheckerTest extends FrontendTest {

    @Test
    public void negTestOk() {
        assertNoTypeErrors("{ Bool b; b = ~True; }");
    }
    
    @Test
    public void negTestError() {
        Model m = assertParseOkStdLib("{ Bool b; b = ~5; }");
        assertEquals(ErrorMessage.EXPECTED_TYPE,m.typeCheck().getFirst().msg);
    }

    private void assertNoTypeErrors(String absCode) {
        Model m = assertParseOkStdLib(absCode);
        String msg = "";
        SemanticErrorList l = m.typeCheck();
        if (!l.isEmpty())
            msg = l.getFirst().getMsgString();
        assertTrue(msg,l.isEmpty());
    }



}
