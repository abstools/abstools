package abs.frontend.typesystem;

import static org.junit.Assert.*;

import org.junit.Test;

import abs.frontend.FrontendTest;
import abs.frontend.analyser.ErrorMessage;
import abs.frontend.analyser.SemanticErrorList;
import abs.frontend.ast.Model;

public class TypeCheckerTest extends FrontendTest {

	 // POSITIVE TESTS
	
	
	 @Test
	 public void testVarDecl() {
		 assertNoTypeErrors("{ Bool b = True; }"); 
	 }

	 @Test
	 public void testVarDeclInit() {
		 assertNoTypeErrors("interface I {} interface J extends I {} { J j; I i = j; }"); 
	 }

     @Test
     public void testClass() {
         assertNoTypeErrors("interface I {} class C implements I {} { I i; i = new C(); }"); 
     }

     @Test
     public void testClass2() {
         assertNoTypeErrors("interface I {} interface J {} class C implements I,J {} { J j; j = new C(); }"); 
     }
     
    @Test
    public void negTestOk() {
        assertNoTypeErrors("{ Bool b = ~True; }");
    }

    @Test
    public void andOk() {
        assertNoTypeErrors("{ Bool b = True && False;  }");
    }

    @Test
    public void plusOk() {
        assertNoTypeErrors("{ Int i = 4 + 5; }");
    }
    
    @Test
    public void getOk() {
        assertNoTypeErrors("{ Fut<Bool> f; f.get; }");
    }

    @Test
	 public void letOk() {
		 assertNoTypeErrors("{ Bool b = let (Bool x) = True in x; }"); 
	 }
    
    @Test
	 public void caseOk() {
		 assertNoTypeErrors("{ Bool x = True; Bool b = case x { True => False; False => True; }; }"); 
	 }

    @Test
	 public void caseVarOk() {
		 assertNoTypeErrors("data Foo = Bar(Bool); { Foo x = Bar(True);" +
		 		" Bool b = case x { Bar(y) => y; }; }"); 
	 }
    
    
    @Test
    public void methodEmptyOk() {
        assertNoTypeErrors("class C { Unit m() { } }");
    }
    
    @Test
    public void methodNoReturnOk() {
        assertNoTypeErrors("class C { Unit m() { Bool b = True; b = False; } }");
    }

    @Test
    public void methodReturnOk() {
        assertNoTypeErrors("class C { Unit m() { Bool b = True; return Unit; } }");
    }
    
    @Test
	 public void testIfOk() {
		 assertNoTypeErrors("{ if (True) { } else { } }"); 
	 }
    
    @Test
	 public void testWhileOk() {
		 assertNoTypeErrors("{ while (True) { } }"); 
	 }
    
    @Test
	 public void testAwaitClaimOk() {
		 assertNoTypeErrors("{ Fut<Bool> f; await f?; }"); 
	 }
    
    @Test
	 public void testAwaitBoolOk() {
		 assertNoTypeErrors("{ Bool b = False; await b; }"); 
	 }
    
    @Test
	 public void testAwaitAndOk() {
		 assertNoTypeErrors("{ await False && True; }"); 
	 }

    
    
    // NEGATIVE TESTS
    
	 @Test
	 public void testUnresolvableType() {
		 assertTypeErrors("{ I i; }"); 
	 }

    
    @Test
    public void testClassError() {
        assertTypeErrors("interface I {} interface J{} class C implements J {} { I i; i = new C(); }"); 
    }
    
    @Test
    public void dataTypeDuplicateConstructors() {
        assertNoTypeErrors("data Foo = Bar | Bar ;"); 
    }

    
    @Test
    public void negTestError() {
        Model m = assertParseOkStdLib(" { Bool b = ~5; }");
        assertEquals(ErrorMessage.EXPECTED_TYPE,m.typeCheck().getFirst().msg);
    }

    @Test
    public void plusError() {
        assertTypeErrors("{ Int i = 4 + True; }");
    }

    @Test
    public void getError() {
        assertTypeErrors("{ Bool f = True; f.get; }");
    }
    
    @Test
    public void orError() {
        assertTypeErrors("{ Bool b = True || 5; }");
    }
    
    @Test
    public void andError() {
        assertTypeErrors("{ Bool b = 5 && True; }");
    }
    
    @Test
	 public void letError() {
		 assertTypeErrors("{ Bool b = let (Bool x) = 5 in x; }"); 
	 }
    
    @Test
	 public void caseError() {
		 assertTypeErrors("{ Bool x = True; Bool b = case x { True => False; False => 5; }; }"); 
	 }
    
    @Test
	 public void caseErrorNoDataType() {
		 assertTypeErrors("interface I { } { I i; Bool b = case i { True => False; False => 5; }; }"); 
	 }

    @Test
	 public void caseErrorConstructorNotResolvable() {
		 assertTypeErrors("{ Bool x = True; Bool b = case x { Foo => False; }; }"); 
	 }

    @Test
	 public void caseErrorConstructorWrongArgNum() {
		 assertTypeErrors("{ Bool x = True; Bool b = case x { True(5) => False; }; }"); 
	 }
    
    @Test
	 public void caseErrorConstructorWrongArgType() {
		 assertTypeErrors("data Foo = Bar(Bool);  { Foo x = Bar(True); Bool b = case x { Bar(5) => False; }; }"); 
	 }
    
    @Test
	 public void caseErrorConstructorExpWrongArgType() {
		 assertTypeErrors("data Foo = Bar(Bool); { Foo x = Bar(5); }"); 
	 }
    
    @Test
    public void methodReturnError() {
        assertTypeErrors("class C { Unit m() { return True; } }");
    }
    
    @Test
    public void methodMissingReturn() {
        assertTypeErrors("class C { Bool m() {  } }");
    }
    
    @Test
	 public void testVarDeclInitNoSubtypeError() {
		 assertTypeErrors("interface I {} interface J {} { J j; I i = j; }"); 
	 }

    @Test
	 public void testVarDeclInitMissingError() {
		 assertTypeErrors("{ Bool b; }"); 
	 }
    
    @Test
	 public void testIfNoBool() {
		 assertTypeErrors("{ if (5) { } else { } }"); 
	 }

    @Test
	 public void testWhileNoBool() {
		 assertTypeErrors("{ while (5) { } }"); 
	 }
    
    @Test
	 public void testAwaitNoFut() {
		 assertTypeErrors("{ Bool b = True; await b?; }"); 
	 }
    
    @Test
	 public void testAwaitNoBool() {
		 assertTypeErrors("{ await 5; }"); 
	 }

    @Test
	 public void testAwaitAndError() {
		 assertTypeErrors("{ await 5 && True; }"); 
	 }
    
    
    private void assertNoTypeErrors(String absCode) {
   	 assertTypeErrors(absCode, false);
    }

    private void assertTypeErrors(String absCode) {
   	 assertTypeErrors(absCode, true);
    }

    private void assertTypeErrors(String absCode, boolean expectErrors) {
       Model m = assertParseOkStdLib(absCode);
       String msg = "";
       SemanticErrorList l = m.typeCheck();
       if (!l.isEmpty())
           msg = l.getFirst().getMsgString();
       assertEquals(msg,expectErrors, !l.isEmpty());
   }


}
