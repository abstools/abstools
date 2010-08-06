package abs.frontend.analyser;

import static abs.frontend.analyser.ErrorMessage.DUPLICATE_TYPE_DECL;
import static abs.frontend.analyser.ErrorMessage.DUPLICATE_BEHAVIOR_DECL;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

import abs.frontend.FrontendTest;
import abs.frontend.ast.Model;


public class DuplicateCheckTest extends FrontendTest {

	@Test
	public void interfaceDuplicates() {
		Model p = assertParseOk("interface I {} interface I {}"); 
		assertTrue(p.getErrors().size() == 1);
		assertEndsWith(p.getErrors().getFirst(), DUPLICATE_TYPE_DECL.withArgs("I"));
	}
	
	@Test
	public void datatypeDuplicates() {
		Model p = assertParseOk("data D; data D;");
		assertTrue(p.getErrors().size() == 1);
		assertEndsWith(p.getErrors().getFirst(), DUPLICATE_TYPE_DECL.withArgs("D"));
	}
	
	@Test
	public void duplicateClasses() {
		Model p = assertParseOk("class C {} class C {}"); 
		assertTrue(p.getErrors().size() == 1);
		assertEndsWith(p.getErrors().getFirst(), DUPLICATE_BEHAVIOR_DECL.withArgs("C"));
	}
	
	@Test
	public void duplicateFunctions() {
		Model p = assertParseOk("data Int; def Int zero() = 0; def Int zero() = 0;"); 
		assertTrue(p.getErrors().size() == 1);
		assertEndsWith(p.getErrors().getFirst(), DUPLICATE_BEHAVIOR_DECL.withArgs("zero"));
	}
	
    private void assertEndsWith(SemanticError expected, String actual) {
        assertTrue("Expected that "+expected.getMsgString()+" ends with "+actual,expected.getMsgString().endsWith(actual));
    }

}
