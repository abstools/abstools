
package tests;

//import junit.framework.*;
import abs.frontend.ast.*;
import abs.frontend.parser.*;
import beaver.*;
import java.io.*;

import org.junit.Before; 
import org.junit.Ignore; 
import org.junit.Test; 
import static org.junit.Assert.*;

public class ParserTest {


	private String rest ; 
	
	@Before
        public void setUp() {
		rest = new String("{   return null   }"); 
	}


	
	
	@Test
		public void testSimpleBlock() {
		assertParseOk(rest); 
		assertParseOk("{   return x.get   }"); 
		assertParseOk("{   skip ; return x.get   }") ;
		assertParseOk("{ Int x , Int y ;  skip ; return x.get   }");
		assertParseError("{   ; return x.get   }") ;
		assertParseError("{   ; skip  ; return x.get   }") ;
		assertParseError("{ }");
	}
	
	// Class declarations
	@Test
		public void testIfDecl() {
		assertParseOk("interface Foo {}" + rest );
		assertParseOk("interface Foo extends Foo1 , Foo2 {}" + rest );
	}
	
	//TODO more testcases 

	//@Test
		public void testClassDeclWithExtends() {
		assertParseOk("{ class A extends B { } }");
	}
	//@Test
		public void testClassDeclWithQualifiedExtends() { // TODO: should this be valid?
		assertParseError("{ class A extends A.B { } }");
	}
	//@Test
		public void testNestedClassDecl() {
		assertParseOk("{ class A { class B { } } }");
	}
	
	// Variable declarations
	//@Test
		public void testVarDecl() {
		assertParseOk("{ A a; }");
	}
	//@Test
		public void testVarDeclQualifiedType() {
		assertParseOk("{ A.B.C a; }");
	}
	//@Test
		public void testVarDeclComplexName() {
    assertParseError("{ A.B.C a.b; }");
	}
	
	// Assignment
	//@Test
		public void testAssignStmt() {
		assertParseOk("{ a = b; }");
	}
	//@Test
		public void testAssignStmtQualifiedLHS() {
		assertParseOk("{ a.b.c = b; }");
	}
	//@Test
		public void testAssignStmtQualifiedRHS() {
		assertParseOk("{ a = b.c.d; }");
	}
	
	// While statement
	//@Test
		public void testWhileStmt() {
		assertParseOk("{ while ( a ) a = b; }");
	}
	//@Test
		public void testWhileStmtBlock() { // TODO: should this be valid?
		assertParseError("{ while ( a ) { a = b; } }");
	}
	
	protected static void assertParseOk(String s) {
		try {
			parse(s);
		} catch (Throwable t) {
			fail(t.getMessage());
		}
  }
	
	protected static void assertParseError(String s) {
		try {
			parse(s);
		} catch (Throwable t) {
			return;
		}
		fail("Expected to find parse error in " + s);
	}
	
	protected static void parse(String s) throws Throwable {
		ABSParser parser = new ABSParser();
		Reader reader = new StringReader(s);
		ABSScanner scanner = new ABSScanner(new BufferedReader(reader));
		Program p = (Program)parser.parse(scanner);
		reader.close();
	}
}
