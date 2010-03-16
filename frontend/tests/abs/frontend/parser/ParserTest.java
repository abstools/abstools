
package abs.frontend.parser;

//import junit.framework.*;
import static org.junit.Assert.fail;

import java.io.BufferedReader;
import java.io.Reader;
import java.io.StringReader;

import org.junit.Before;
import org.junit.Test;

import abs.frontend.ast.Program;

public class ParserTest {


	private String emptyblock ; 	
	private String decls, ifdecl1, ifdecl2  , cldecl1,   cldecl2 , cldecl3 ; 
	private String ms1, ms2, meth1, meth2 , fields, comment, comment2  ; 
	

	private static boolean verbose = false ; 

	private String[] assignPure = 
	{"x = y ; ", 
	 "x = null ; ",  
	 "x = y.get ; ", 
	 "this.x = y.get ; ", 
	 //"x = y == z",
	 };
	
	
	private String[] assignEff = 	
	{"x = new Foo() ; ", 
	 "x = new Foo(a,b) ; ", 
	 "x = o!init() ; ", 
	 "x = o!init(y) ; ", 
	 "x = o!init(y,z) ; ", 
	 "x = o.init(y,z,w) ; ", 
	 "x = init(y,z) ; "} ; 
	
	private String[] awaitStmt = 	
	{"await y? ; " ,
	 "await y? & z? ; " ,
	 "await y? & z? & w? ; ", 
	 "await y ; " //using pure exp v as guard. 
	};
	
	private		String[] otherStmt = 	
	{"skip ; ",
	 "release ; "
	};
	

 
	@Before
        public void setUp() {
		
		emptyblock = "{    }"; 
		//methodsignatures 
		ms1 = "Void init(Foo x ,  Bar y)";
		ms2 = "Void append(Int i)";
		meth1 = ms1 + "{ Int x ;  Int y ;	return null ; }";
	    meth2 = ms2 + "{ skip; return null ; }";
		fields = "ListofInt buffer ;     Int max ;      Int n 	;"; 

		ifdecl1 = " interface Foo {}";
		ifdecl2 = " interface Bar extends Bar1, Bar2 {}";
		cldecl1 = " class FooClass  {}";
		cldecl2 = " class FooClass implements Foo {}";
		cldecl3 = "\nclass BoundedBuffer implements Buffer { \n" + fields + ";" + "\n" + meth1 + "\n" + meth2 + "\n" + "}";
		decls = ifdecl1 + "\n" + ifdecl2  + "\n" + cldecl1 + "\n" +   cldecl2 + "\n" + cldecl3 ; 

		

		
		
   	}
		
	@Test
		public void testBlock() {
		assertParseOk(" "); 		//NO decls no block 
		assertParseOk("{ }"); //No decls. 
		assertParseOk("{   return x.get ;   }"); //one statement
		assertParseOk("{   skip ; return x.get ;    }") ;  //n statements
		assertParseOk("{ Int x ; Int y ;  skip ; return x.get ;   }"); //Variable decls
		assertParseOk("{  ; ; ; ;  ; return x.get ;   }") ;
		assertParseOk("{   ; skip  ; return x.get ;  }") ;
  		assertParseOk("{ Fut(I) x ; J z ; }") ;	//need trailing semicolon here. 
		assertParseOk("{ Fut(I) x ; Fut(Fut(I)) y ;  J z ; K w  ; }") ;	
		//
}
	
	// Interface declarations
	@Test
		public void testIfDecl() {
		assertParseOk(" interface Foo {} {}");
		assertParseOk(" interface Bar extends Bar1, Bar2 {} {}" );
		assertParseError("interface Foo extends {} {}" );
		assertParseError("interface extends {} {}" );
	}



	// Class declarations
	@Test
		public void testClassDecl() {
		assertParseOk("class FooClass  {} {}"); 
		assertParseOk("class FooClass  implements Foo {} {}");
		assertParseOk("class FooClass  implements Foo {}"); //optional main body 
		assertParseOk("class FooClass(T x , T y)  implements Foo {}"); //class params 
		assertParseOk("class FooClass(T x)  implements Foo {}"); //class params 
		assertParseOk("class FooClass()  implements Foo {}"); //class params 
		assertParseOk("class FooClass  implements Foo { { x = a ; } T x  }"); //init block
		assertParseOk("class FooClass  implements Foo { {} } {} "); //empty init block
		assertParseOk("class BoundedBuffer implements Buffer { \n"+
					  "  ListofInt buffer ;     Int max ;      Int n 	\n"+
					  "  Void init(Foo x){ Int x ;  Int y ;	return null ; }\n"+
					  "  Void append(Int i){ skip; return null ; }}" + "{}"
					  );
		assertParseError("class FooClass implements {}" + "{}" );
	   
	}
	

	// datatype declarations
	@Test
		public void testDatatypeDecl() {
		assertParseOk("data Bool { True , False }"); 
		assertParseOk("data IntList { IntNil , Cons(Int, IntList)}");
	}

	//@Test
		public void testFunctionDecl() {
		assertParseOk("def Int inc(Int x) = x"); //fun_exp = IDENTIFIER 
		assertParseOk("def Int inc(Int x) = plus(x,one())"); //fun_exp = Term(co,l)
		assertParseOk("def Datatype fn(Int x , Int y) = x"); 
		//		assertParseOk("data IntList { IntNil , Cons(Int, IntList)}");
	}

	


	// comments
	@Test
		public void testComment() {
		assertParseOk("// one line\n");
		assertParseOk("/* Multi \n line \n comment */");
	}
	

	@Test
		public void testProg() {
		assertParseOk(decls + "{}" );
	}



	@Test
		public void testStmts() {
		System.out.println(assignPure);
		for (String s : assignPure)	assertParseOk("{"+ s + "}");
  		for (String s : assignEff)	assertParseOk("{"+ s + "}");
		for (String s : awaitStmt)	assertParseOk("{"+ s + "}");
		for (String s : otherStmt)	assertParseOk("{"+ s + "}");
		
	}		

	@Test
	public void testIfStmts() {
		
		assertParseOk("{ if (x) y = true ; return null ; }") ; 
		assertParseOk("{ if (x) y = true ; else y = false ; return null ; }") ; 
		assertParseOk("{ if (x) { y = true ; z = false; }  else y = false ; return null ; }") ; 
		assertParseError("{ if x then { y = true ; z = false }  else y = false ; return null ; }") ; 
		
		
		
}	
	
	@Test
		public void testStmtBlock(){
			assertParseOk("{ return null ; }" );
			assertParseError(" { return null }" );
			
			assertParseOk("{ x = y ; return null ; }" ); 
			assertParseError("{ x = y  return null ; }" ); 
			
			assertParseOk("{ x = y ; y = z ; return null ; }" ); 
			
			assertParseOk("{ { x = y ; skip ; await x? ; } ; return null ; }" ); 
			assertParseOk("{ { x = y ; } ; return null ; }" );
			assertParseOk(" { { } ; return null ; }  " );
		 	
	}
			

	@Test
	public void testStmtList() {
				assertParseOk("{ x = null; x = y.get ; x = y ; } ");
				
	}

	protected static void assertParseOk(String s) {
		try {
			if (verbose) 
				System.out.println("Assert OK: "+s);
			parse(s);
		} catch (Throwable t) {
			//fail("something failed");
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
		//		ABSScanner scanner = new ABSScanner(new BufferedReader(reader));
		ABSScanner scanner = new ABSScanner(reader);
		Program p = (Program)parser.parse(scanner);
		reader.close();
	}
}
