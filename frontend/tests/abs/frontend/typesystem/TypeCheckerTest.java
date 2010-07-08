package abs.frontend.typesystem;

import static org.junit.Assert.*;

import org.junit.Test;

import abs.frontend.FrontendTest;
import abs.frontend.analyser.ErrorMessage;
import abs.frontend.analyser.SemanticErrorList;
import abs.frontend.ast.Model;

public class TypeCheckerTest extends FrontendTest {

	 // POSITIVE TESTS
	
	 @Test
	 public void testVarDecl() {
		 assertNoTypeErrors("{ Bool b = True; }"); 
	 }

	 @Test
	 public void testVarDeclInit() {
		 assertNoTypeErrors("interface I {} interface J extends I {} { J j; I i = j; }"); 
	 }
	 
    @Test
    public void negTestOk() {
        assertNoTypeErrors("{ Bool b = ~True; }");
    }

    @Test
    public void andOk() {
        assertNoTypeErrors("{ Bool b = True && False;  }");
    }

    @Test
    public void plusOk() {
        assertNoTypeErrors("{ Int i = 4 + 5; }");
    }
    
    // NEGATIVE TESTS
    
    @Test
    public void negTestError() {
        Model m = assertParseOkStdLib(" { Bool b = ~5; }");
        assertEquals(ErrorMessage.EXPECTED_TYPE,m.typeCheck().getFirst().msg);
    }

    @Test
    public void plusError() {
        assertTypeErrors("{ Int i = 4 + True; }");
    }

    @Test
    public void orError() {
        assertTypeErrors("{ Bool b = True || 5; }");
    }
    
    @Test
    public void andError() {
        assertTypeErrors("{ Bool b = 5 && True; }");
    }
    
    @Test
	 public void testVarDeclInitNoSubtypeError() {
		 assertTypeErrors("interface I {} interface J {} { J j; I i = j; }"); 
	 }

    @Test
	 public void testVarDeclInitMissingError() {
		 assertTypeErrors("{ Bool b; }"); 
	 }
    
    
    private void assertNoTypeErrors(String absCode) {
   	 assertTypeErrors(absCode, false);
    }

    private void assertTypeErrors(String absCode) {
   	 assertTypeErrors(absCode, true);
    }

    private void assertTypeErrors(String absCode, boolean expectErrors) {
       Model m = assertParseOkStdLib(absCode);
       String msg = "";
       SemanticErrorList l = m.typeCheck();
       if (!l.isEmpty())
           msg = l.getFirst().getMsgString();
       assertEquals(msg,expectErrors, !l.isEmpty());
   }


}
