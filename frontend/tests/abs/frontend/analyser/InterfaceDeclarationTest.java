package abs.frontend.analyser;

import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.Reader;
import java.io.StringReader;
import java.util.Iterator;

import org.junit.Test;

import abs.frontend.ast.Model;
import abs.frontend.parser.ABSParser;
import abs.frontend.parser.ABSScanner;

import static abs.frontend.analyser.ErrorMessage.*;


public class InterfaceDeclarationTest {

	@Test
	public void trivial() {
		Model p;
		p = assertParseOk("interface I {} {}");
		assertTrue(p.getErrors().size() == 0);
	}

	@Test
	public void extending() {
		Model p;
		p = assertParseOk("interface I {} interface J extends I {} {}"); 
		assertTrue(p.getErrors().size() == 0);
	}

	@Test
	public void extendingReversed() {
		Model p;
		p = assertParseOk("interface J extends I {} interface I {} {}"); 
		assertTrue(p.getErrors().size() == 0);
	}

	@Test
	public void extendingUndefined() {
		Model p;
		p = assertParseOk("interface J extends I {} {}"); 
		assertTrue(p.getErrors().size() == 1);
		assertEndsWith(p.getErrors().getFirst(),UNKOWN_INTERFACE.withArgs("I"));
	}

	@Test
	public void circular() {
		Model p;
		p = assertParseOk("interface I extends I {} {}"); 
		assertTrue(p.getErrors().size() == 1);
		assertEndsWith(p.getErrors().getFirst(),CYCLIC_INHERITANCE.withArgs("I"));
	}

    @Test
	public void mutuallyCircular() {
		Model p;
		p = assertParseOk("interface I extends J {} interface J extends I {} {}"); 
		assertTrue(p.getErrors().size() == 2);
		Iterator<SemanticError> i = p.getErrors().iterator();
		assertEndsWith(i.next(),CYCLIC_INHERITANCE.withArgs("I"));
		assertEndsWith(i.next(),CYCLIC_INHERITANCE.withArgs("J"));
	}

	@Test
	public void mutuallyCircularIndirect() {
		Model p;
		p = assertParseOk("interface I extends J {}  interface J extends K {}  interface K extends I {}"); 
		assertTrue(p.getErrors().size() == 3);
		Iterator<SemanticError> i = p.getErrors().iterator();
		assertEndsWith(i.next(),CYCLIC_INHERITANCE.withArgs("I"));
		assertEndsWith(i.next(),CYCLIC_INHERITANCE.withArgs("J"));
		assertEndsWith(i.next(),CYCLIC_INHERITANCE.withArgs("K"));
	}

	
    private void assertEndsWith(SemanticError expected, String actual) {
        assertTrue("Expected that "+expected.getMsgString()+" ends with "+actual,expected.getMsgString().endsWith(actual));
    }

	// TODO refactor this into testframework stuff? 
	protected static Model assertParseOk(String s) {
		Model p = null;
		try {
			p = parse(s);
		} catch (Throwable t) {
			fail("Failed to parse: "+ s+"\n"+t.getMessage());
		}
		return p;
  }
	
	
	protected static Model parse(String s) throws Throwable {
		ABSParser parser = new ABSParser();
		Reader reader = new StringReader(s);
		//		ABSScanner scanner = new ABSScanner(new BufferedReader(reader));
		ABSScanner scanner = new ABSScanner(reader);
		Model p = (Model)parser.parse(scanner);
		reader.close();
		return p;
	}
}
