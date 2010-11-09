package abs.frontend.analyser;

import static abs.frontend.analyser.ErrorMessage.*;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

import abs.frontend.FrontendTest;
import abs.frontend.ast.Model;


public class DuplicateCheckTest extends FrontendTest {

	@Test
	public void interfaceDuplicates() {
		Model p = assertParseOk("interface I {} interface I {}");
		assertEndsWith(p.typeCheck().getFirst(), DUPLICATE_TYPE_DECL.withArgs("I"));
	}
	
	@Test
	public void datatypeDuplicates() {
		Model p = assertParseOk("data D; data D;");
		assertEndsWith(p.typeCheck().getFirst(), DUPLICATE_TYPE_DECL.withArgs("D"));
	}

    @Test
    public void typeDuplicates() {
        Model p = assertParseOk("data D; interface D { }");
        assertEndsWith(p.typeCheck().getFirst(), DUPLICATE_TYPE_DECL.withArgs("D"));
    }

    @Test
    public void typeDuplicates2() {
        Model p = assertParseOk("data D; data X; type D = X;");
        assertEndsWith(p.typeCheck().getFirst(), DUPLICATE_TYPE_DECL.withArgs("D"));
    }
    
	@Test
	public void duplicateClasses() {
		Model p = assertParseOk("class C {} class C {}"); 
		assertEndsWith(p.typeCheck().getFirst(), DUPLICATE_CLASS_NAME.withArgs("C"));
	}
	
	@Test
	public void duplicateFunctions() {
		Model p = assertParseOk("data Test = Test; def Test zero() = Test; def Test zero() = Test;"); 
		assertEndsWith(p.typeCheck().getFirst(), DUPLICATE_FUN_NAME.withArgs("zero"));
	}
	
    private void assertEndsWith(SemanticError expected, String actual) {
        assertTrue("Expected that "+expected.getHelpMessage()+" ends with "+actual,expected.getHelpMessage().endsWith(actual));
    }

}
