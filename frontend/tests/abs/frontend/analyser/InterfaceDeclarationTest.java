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

public class InterfaceDeclarationTest {

	@Test
	public void trivial() {
		Model p;
		p = assertParseOk("interface I {} {}");
		assertTrue(p.errors().size() == 0);
	}

	@Test
	public void extending() {
		Model p;
		p = assertParseOk("interface I {} interface J extends I {} {}"); 
		assertTrue(p.errors().size() == 0);
	}

	@Test
	public void extendingReversed() {
		Model p;
		p = assertParseOk("interface J extends I {} interface I {} {}"); 
		assertTrue(p.errors().size() == 0);
	}

	@Test
	public void extendingUndefined() {
		Model p;
		p = assertParseOk("interface J extends I {} {}"); 
		assertTrue(p.errors().size() == 1);
	    assertTrue(((String)p.errors().iterator().next()).endsWith("Unknown interface: I"));
	}

	@Test
	public void circular() {
		Model p;
		p = assertParseOk("interface I extends I {} {}"); 
		assertTrue(p.errors().size() == 1);
	    assertTrue(((String)p.errors().iterator().next()).endsWith("Cyclic inheritance chain for interface: I"));
	}

	@Test
	public void mutuallyCircular() {
		Model p;
		p = assertParseOk("interface I extends J {} interface J extends I {} {}"); 
		assertTrue(p.errors().size() == 2);
		Iterator<?> i = p.errors().iterator();
	    assertTrue(((String)i.next()).endsWith("Cyclic inheritance chain for interface: I"));
	    assertTrue(((String)i.next()).endsWith("Cyclic inheritance chain for interface: J"));
	}

	@Test
	public void mutuallyCircularIndirect() {
		Model p;
		p = assertParseOk("interface I extends J {}  interface J extends K {}  interface K extends I {}"); 
		assertTrue(p.errors().size() == 3);
		Iterator<?> i = p.errors().iterator();
	    assertTrue(((String)i.next()).endsWith("Cyclic inheritance chain for interface: I"));
	    assertTrue(((String)i.next()).endsWith("Cyclic inheritance chain for interface: J"));
	    assertTrue(((String)i.next()).endsWith("Cyclic inheritance chain for interface: K"));
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
