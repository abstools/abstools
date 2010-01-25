
package tests;

import abs.frontend.ast.*;
import abs.frontend.parser.*;
import java.io.*;
import beaver.*;

import org.junit.Before; 
import org.junit.Ignore; 
import org.junit.Test; 
import static org.junit.Assert.*;

public class ScannerTest {

	@Test
	public void testValidKeywords() {
		assertScannerOk("class extends while");
	}
	
	@Test
		public void testValidSeparators() {
		assertScannerOk("{} () ; .");
	}
	
	@Test 
		public void testValidOperators() {
		assertScannerOk("=");
  }
	@Test 
		public void testValidLiterals() {
		assertScannerOk("true false null");
  }
  @Test 
	  public void testValidIdentifiers() {
	  assertScannerOk("a ab a1 a1b a1b1");
  }
	@Test 
		public void testValidComments() {
		assertScannerOk("// !@#$%^&*abc");
	}

	@Test
		public void testInvalidTokens() {
		assertScannerError("@");
		assertScannerError("!");
		assertScannerError("$");
		assertScannerError("\\");
		assertScannerError("#");
		assertScannerError("<");
		assertScannerError(">");
		assertScannerError("_a");
		assertScannerError("1");
		assertScannerError("1a");
		assertScannerError("// !@#$%^&*abc \n !");
		assertScannerError("/* abc */");
	}

	
	protected static void assertScannerOk(String s) {
		try {
			scan(s);
		} catch (Throwable t) {
			fail(t.getMessage());
		}
	}

   protected static void assertScannerError(String s) {
     try {
       scan(s);
     } catch (Throwable t) {
       return;
     }
     fail("Expected to find parse error in " + s);
   }
	

   protected static void scan(String s) throws Throwable {
     Reader reader = new StringReader(s);
     ABSScanner scanner = new ABSScanner(new BufferedReader(reader));
     Symbol symbol;
     do {
       symbol = scanner.nextToken();
     } while (symbol.getId() != ABSParser.Terminals.EOF);
     reader.close();
   }
}

	









  
//   // utilitity asserts to test scanner
  
//   protected static void assertScannerOk(String s) {
//     try {
//       scan(s);
//     } catch (Throwable t) {
//       fail(t.getMessage());
//     }
//   }
  
  
