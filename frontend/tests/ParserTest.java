
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


	private String emptyblock ; 	
	private String decls, ifdecl1, ifdecl2  , cldecl1,   cldecl2 , cldecl3 ; 
	private String ms1, ms2, ms3 , meth1, meth2 , fields, comment, comment2  ; 
	
	private String prestmt = "{" ; 
	private String poststmt = " return null }" ; 

	private static boolean verbose = true ; 

	private String[] assignPure = 
	{"x = y", 
	 "x = null",  
	 "x = y.get", 
	 "x = ~y",  
	 "x = y && z",
	 "x = y || z",  
	 "x = y == z",
	 "x = true",
	 "x = false"};
	
	
	private String[] assignEff = 	
	{"x = new Foo()", 
	 "x = o!init()", 
	 "x = o!init(y)", 
	 "x = o!init(y,z)", 
	 "x = o.init(y,z,w)", 
	 "x = init(y,z)"} ; 
	
	private String[] awaitStmt = 	
	{"await y?" ,
	 "await y? & z?" ,
	 "await y? & z? & w?"};
	
	private		String[] otherStmt = 	
	{"skip",
	 "release"};
	
	private 	String[] ifStmt = 
	{"if x then y = true",
	 "if x then y = true else y = false",
	 "if x then y = true else { y = false ; x = null } "};
	
	
	private	 String[] stmtBlock = 
	{"{ x = y ; skip ; await x?  }", 
	 "{  }"};
 
	
	@Before
        public void setUp() {
		
		emptyblock = "{   return null   }"; 
		//methodsignatures 
		ms1 = "Void init(Foo x, Bar y)";
		ms2 = "Void append(Int i)";
		ms3 = "Data remove()";
		
		meth1 = ms1 + "{ Int x, Int y ;	return null }";
	    meth2 = ms2 + "{ skip; return null}";
		fields = "ListofInt buffer,     Int max ,     Int n 	;"; 

		comment = "// one line";
		comment2 = "/* Multi \n line \n comment */";
		ifdecl1 = " interface Foo {}";
		ifdecl2 = " interface Bar extends Bar1, Bar2 {}";
		cldecl1 = " class FooClass  {}";
		cldecl2 = " class FooClass implements Foo {}";
		cldecl3 = "\nclass BoundedBuffer implements Buffer { \n" + fields + "\n" + meth1 + "\n" + meth2 + "\n" + "}";
		decls = ifdecl1 + "\n" + ifdecl2   + "\n" + cldecl1 + "\n" +   cldecl2 + "\n" + cldecl3 ; 

		

		
		
   	}
		
	@Test
		public void testBlock() {
		assertParseOk(emptyblock); 
		assertParseOk("{   return x.get   }"); 
		assertParseOk("{   skip ; return x.get   }") ;
		assertParseOk("{ Int x , Int y ;  skip ; return x.get   }");
		assertParseError("{   ; return x.get   }") ;
		assertParseError("{   ; skip  ; return x.get   }") ;
		assertParseError("{ }");
	}
	
	// Interface declarations
	@Test
		public void testIfDecl() {
		assertParseOk(ifdecl1 + emptyblock );
		assertParseOk(ifdecl2 + emptyblock );
		assertParseError("interface Foo extends {}" + emptyblock );
		assertParseError("interface extends {}" + emptyblock );
	}



	// Class declarations
	@Test
		public void testClassDecl() {
		assertParseOk(cldecl1 + emptyblock );
		assertParseOk(cldecl2 + emptyblock );
		assertParseOk(cldecl3 + emptyblock );
		assertParseError("class FooClass implements {}" + emptyblock );
	}
	

	@Test
		public void testProg() {
		assertParseOk(decls + emptyblock );
	}



	@Test
		public void testStmts() {
		//		System.out.println(assignPure);
		for (String s : assignPure)	assertParseOk(prestmt + s + ";" + poststmt); 
		for (String s : assignEff)	assertParseOk(prestmt + s + ";" + poststmt); 
		for (String s : awaitStmt)	assertParseOk(prestmt + s + ";" + poststmt); 
		for (String s : otherStmt)	assertParseOk(prestmt + s + ";" + poststmt); 
		for (String s : ifStmt)	assertParseOk(prestmt + s + ";" + poststmt); 
		for (String s : stmtBlock)	assertParseOk(prestmt + s + ";" + poststmt); 
	}		

	@Test
		public void testStmtList() {
				assertParseOk(prestmt + "x = null; x = y.get ; x = ~y ; " + poststmt); 
				assertParseError(prestmt + ";" + poststmt); 
				
	}

	//TODO more testcases 
	protected static void assertParseOk(String s) {
		try {
			if (verbose) 
				System.out.println("Assert OK: "+s);
			parse(s);
		} catch (Throwable t) {
			fail("Failed to parse: "+ s+"\n"+t.getMessage());
		}
  }
	
	protected static void assertParseError(String s) {
		try {
			if (verbose) 
				System.out.println("Assert Error: "+s);
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
