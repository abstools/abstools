package tests;

//import junit.framework.*;
import abs.frontend.ast.*;
import beaver.*;

import org.junit.Before; 
import org.junit.Ignore; 
import org.junit.Test; 
import static org.junit.Assert.*;

public class ParserTest {


	//   public void testValidKeywords() {
	//  assertScannerOk("class extends while");
	//}

	private String foo = null;  

	@Before
        public void setUp() {
		foo = new String("Foo"); 
	}



   @Test
   public void parserFirst() {
	
	   //TODO: call assertOK or assertError with parameters to test. 
	   assertNotNull("ErrMSG", foo);
	   
   }

   @Test
   public void parserSecond() {
	
	   assertNull("ErrMsg", foo);
	   
   }



}
