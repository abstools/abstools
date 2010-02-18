package abs.frontend.analyser;

import static org.junit.Assert.*;
import static org.junit.Assert.fail;

import java.io.BufferedReader;
import java.io.Reader;
import java.io.StringReader;
import java.util.Iterator;

import org.junit.Before;
import org.junit.Test;

import abs.frontend.ast.Program;
import abs.frontend.parser.ABSParser;
import abs.frontend.parser.ABSScanner;

public class InterfaceDeclarationTest {

	@Test
	public void trivial() {
		Program p;
		p = assertParseOk("interface I {} {}");
		assertTrue(p.errors().size() == 0);
	}

	@Test
	public void extending() {
		Program p;
		p = assertParseOk("interface I {} interface J extends I {} {}"); 
		assertTrue(p.errors().size() == 0);
	}

	@Test
	public void extendingReversed() {
		Program p;
		p = assertParseOk("interface J extends I {} interface I {} {}"); 
		assertTrue(p.errors().size() == 0);
	}

	@Test
	public void extendingUndefined() {
		Program p;
		p = assertParseOk("interface J extends I {} {}"); 
		assertTrue(p.errors().size() == 1);
	    assertEquals("Unknown identifier I", p.errors().iterator().next());
	}

	@Test
	public void circular() {
		Program p;
		p = assertParseOk("interface I extends I {} {}"); 
		assertTrue(p.errors().size() == 1);
	    assertEquals("Cyclic inheritance chain for interface I", p.errors().iterator().next());
	}

	@Test
	public void mutuallyCircular() {
		Program p;
		p = assertParseOk("interface I extends J {} interface J extends I {} {}"); 
		assertTrue(p.errors().size() == 2);
		Iterator i = p.errors().iterator();
	    assertEquals("Cyclic inheritance chain for interface I", i.next());
	    assertEquals("Cyclic inheritance chain for interface J", i.next());
	}

	// TODO refactor this into testframework stuff? 
	protected static Program assertParseOk(String s) {
		Program p = null;
		try {
			p = parse(s);
		} catch (Throwable t) {
			//fail("something failed");
			fail("Failed to parse: "+ s+"\n"+t.getMessage());
		}
		return p;
  }
	
	
	protected static Program parse(String s) throws Throwable {
		ABSParser parser = new ABSParser();
		Reader reader = new StringReader(s);
		//		ABSScanner scanner = new ABSScanner(new BufferedReader(reader));
		ABSScanner scanner = new ABSScanner(reader);
		Program p = (Program)parser.parse(scanner);
		reader.close();
		return p;
	}
}
