package testframework;

import tests.*;
import junit.framework.*;
import org.junit.runner.*;
import abs.frontend.ast.*;
import beaver.*;

public class TestRunner {

   public static void main(String[] args) {

	   String[] testCases = {"tests.ScannerTest", "tests.ParserTest"};
	   org.junit.runner.JUnitCore.main(testCases);
	   

   }
}





// package tests;

// import AST.*;
// import java.io.*;
// import junit.framework.TestCase;
// import beaver.*;

// public class ScannerTests extends TestCase {

//   public void testValidKeywords() {
//     assertScannerOk("class extends while");
//   }
//   public void testValidSeparators() {
//     assertScannerOk("{} () ; .");
//   }
//   public void testValidOperators() {
//     assertScannerOk("=");
//   }
//   public void testValidLiterals() {
//     assertScannerOk("true false null");
//   }
//   public void testValidIdentifiers() {
//     assertScannerOk("a ab a1 a1b a1b1");
//   }
//   public void testValidComments() {
//     assertScannerOk("// !@#$%^&*abc");
//   }
  
//   public void testInvalidTokens() {
//     assertScannerError("@");
//     assertScannerError("!");
//     assertScannerError("$");
//     assertScannerError("\\");
//     assertScannerError("#");
//     assertScannerError("<");
//     assertScannerError(">");
//     assertScannerError("_a");
//     assertScannerError("1");
//     assertScannerError("1a");
//     assertScannerError("// !@#$%^&*abc \n !");
//     assertScannerError("/* abc */");
//   }


  
//   // utilitity asserts to test scanner
  
//   protected static void assertScannerOk(String s) {
//     try {
//       scan(s);
//     } catch (Throwable t) {
//       fail(t.getMessage());
//     }
//   }
  
//   protected static void assertScannerError(String s) {
//     try {
//       scan(s);
//     } catch (Throwable t) {
//       return;
//     }
//     fail("Expected to find parse error in " + s);
//   }
  
//   protected static void scan(String s) throws Throwable {
//     Reader reader = new StringReader(s);
//     PicoJavaScanner scanner = new PicoJavaScanner(new BufferedReader(reader));
//     Symbol symbol;
//     do {
//       symbol = scanner.nextToken();
//     } while (symbol.getId() != PicoJavaParser.Terminals.EOF);
//     reader.close();
//   }
// }
