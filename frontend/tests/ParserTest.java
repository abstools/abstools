
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
	private String prestmt, poststmt ; 
	
	@Before
        public void setUp() {
		
		prestmt = "{" ; 
		poststmt = " return null }" ; 
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
		cldecl3 = "class BoundedBuffer implements Buffer { " + fields + meth1 + meth2 + "}";
		decls = ifdecl1 + ifdecl2   + cldecl1 +   cldecl2 + cldecl3 ; 

		
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


// 	@Before
// 		public void setupStmts() {
		
// 		String[] assignPure = 	
// 			{"x = y ;", 
// 			"x = null ;",  
// 			"x = y.get ;", 
// 			"x = ~y ;",  
// 			"x = y && z ;",
// 			"x = y || z ;",  
// 			"x = y == z ;",
// 			"x = true ;",
// 			 "x = false ;"};
		
// 		String[] assignEff = 	
// 			{"x = new Foo() ;", 
// 			 "x = o!init();", 
// 			 "x = o!init(y);", 
// 			"x = o!init(y,z);", 
// 			 "x = o.init(y,z,w);", 
// 			 "x = init(y,z);"} ; 

// 		String[] awaitStmt = 	
// 			"await y? ; 
// 	"await y? & z?; 
// 	"await y? & z? & w?  ;
// 	// No boolean guards
// 	// await true ; 
// 	//skip, release, if_th_else
// 	skip ; 
// 	release ; 
// 	if x then y = true ; 
// 	if x then y = true else y = false ;
// 	if x then y = true else { y = false ; x = null }  ;
	

// 	//Stmtblock
// 	{ x = y ; skip ; await x?  } ;
// 	{  } ;
//     skip ;
// 	//Return  
// new String[]
		
// 		assertParseOk(prestmt + poststmt );
// 		assertParseOk(prestmt + poststmt );
// assertParseOk(prestmt + poststmt );
// assertParseOk(prestmt + poststmt );
// assertParseOk(prestmt + poststmt );
// assertParseOk(prestmt + poststmt );
// assertParseOk(prestmt + poststmt );

		
// 	}


	
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
	
	protected void assertParseOk(String s) {
		try {
			System.out.println("Assert OK:"+s);
			parse(s);
		} catch (Throwable t) {
			fail("Failed to parse: "+ s+"\n"+t.getMessage());
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
