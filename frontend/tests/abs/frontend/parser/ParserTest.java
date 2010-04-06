
package abs.frontend.parser;

//import junit.framework.*;
import static org.junit.Assert.fail;

import java.io.BufferedReader;
import java.io.Reader;
import java.io.StringReader;

import org.junit.Before;
import org.junit.Test;

import abs.frontend.ast.Model;

public class ParserTest {


	private String emptyblock ; 	
	private String bbclass; 
	private String ms1, ms2, meth1, meth2 , fields, comment, comment2  ; 
	

	private static boolean verbose = false ; 

	private String[] pureExp = 
	{" x ", 
	 " this.x ",  
	 "null",
	 "this",
	 //function expressions 
	 //data constructor expression 
	 "Int(x , y)", 
	 //function application 
	 "pluss(x,y)", 
	 "nth(tail(file,n))", 
	 //pair constructor 
	 "pair(x,y)",
	 //case expression 
	"case set { EmptyStringSet() => False ; }"
	};

	
	
	private String[] effExp = 	
	{"new Foo()  ", 
	 "new Foo(a,b)  ", 
	 "o!init()  ", 
	 "o!init(y)  ", 
	 "o!init(y,z)  ", 
	 "this!init(y,z)  ", 
	 "o.init(y,z,w)  ", 
	 //	 "this.init(y,z)", 
	 "y.get"} ; 
	


	private String[] eqExp = { "a == b "} ; 

	

 
	@Before
        public void setUp() {
		
		emptyblock = "{    }"; 
		//methodsignatures 
		ms1 = "Void init(Foo x ,  Bar y)";
		ms2 = "Void append(Int i)";
		meth1 = ms1 + "{ Int x ;  Int y ;	return null ; }";
	    meth2 = ms2 + "{ skip; return null ; }";
		bbclass = "class BoundedBuffer implements Buffer { \n"+
			       "  ListofInt buffer ;     Int max ;      Int n ;	\n"+
			       "  Void init(Foo x){ Int x ;  Int y ;  return null ; }\n"+
			       "  Void append(Int i){ skip; return null ; }}";
		
		
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
		assertParseOk("class FooClass  implements Foo { T x ; { x = a ; }  }"); //init block
		assertParseOk("class FooClass  implements Foo { {} } {} "); //empty init block
		assertParseOk(bbclass);
		assertParseError("class FooClass implements {}" + "{}" );
	   
	}
	

	// datatype declarations
	@Test
		public void testDatatypeDecl() {
		assertParseOk("data Bool { True , False }"); 
		assertParseOk("data IntList { IntNil , Cons(Int, IntList)}");
	}

   @Test
		public void testFunctionDecl() {
		assertParseOk("def Int inc(Int x) = x"); //fun_exp = IDENTIFIER 
		assertParseOk("def Int inc(Int x) = plus(x,one())"); //fun_exp = Term(co,l)
		assertParseOk("def Datatype fn(Int x , Int y) = x"); 
	}

	@Test 
		public void testPureExp(){ 
		//i.e. test pure_exp 
	}


	
	// comments
	@Test
		public void testComment() {
		assertParseOk("// one line\n");
		assertParseOk("/* Multi \n line \n comment */");
	}
	

	@Test
		public void testProg() {
		assertParseOk("interface Foo {} \n interface Bar extends Bar1, Bar2 {} \n class FooClass implements Foo {} {}");
	}



	@Test
		public void testAssignStmts() {
		
		for (String e : pureExp) {
			assertParseOk("{ v = " + e + ";}");
			assertParseOk("{ this.f = " + e + ";}");
		}
		for (String e : effExp) {
			assertParseOk("{ v = " + e + ";}");
			assertParseOk("{ this.f = " + e + ";}");
		}
		for (String e : eqExp) {
			assertParseOk("{ v = " + e + ";}");
			assertParseOk("{ this.f = " + e + ";}");
		}
	}

	@Test
		public void testAwaitStmts() {
		String[] guards = 	
			{"y?", "y? & z? " , " y? & z? & w?", 
			 //any functional expression
			};  
		for (String g : guards)	assertParseOk("{ await "+ g + " ; }");
	}

	@Test
		public void testOtherStmts() {
		String[] otherStmt = 	
			{"skip ; ",
			 "suspend ; ", 
			 "return e ; "
			};
		for (String s : otherStmt)	assertParseOk("{"+ s + "}");
	}		

	@Test
	public void testControlStmts() {
		
		assertParseOk("{ if (x) y = true  ; }") ; 
		assertParseOk("{ if (x) y = true ; else y = false  ; }") ; 
		assertParseOk("{ if (x) { y = true ; z = false; }  else y = false  ; }") ; 
		assertParseOk("{ if (x) { if (y) skip ; else skip ; } else skip ; }") ; 
		assertParseOk("{ if (x) if (y) skip ; else skip ; else skip ; }"); 
		assertParseOk("{ while (x) skip ;  }") ; 
		assertParseOk("{ while (x) { x = y ; skip ; } }") ; 
		assertParseOk("{ if (x) y = true ; else y = false  ; }") ; 
		assertParseOk("{ if (x) { y = true ; z = false; }  else y = false  ; }") ; 
		
	}	
	
	@Test
		public void testStmtBlock(){
			assertParseOk("{ skip ; }" );
			assertParseError(" { skip  }" );
			
			assertParseOk("{ x = y ; skip ; }" ); 
			assertParseError("{ x = y  skip  ; }" ); 
			
			assertParseOk("{ x = y ; y = z ; skip  ; }" ); 
			
			assertParseOk("{ { x = y ; skip ; await x? ; } skip ; }" ); 
			assertParseOk("{ { x = y ; } ; skip ; }" );
			assertParseOk(" { { } { } { } }  " );
		 	
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
		Model p = (Model)parser.parse(scanner);
		reader.close();
	}
}
