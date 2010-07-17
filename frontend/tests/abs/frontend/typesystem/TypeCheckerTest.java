package abs.frontend.typesystem;

import static org.junit.Assert.*;

import org.junit.Test;

import abs.common.StandardLib;
import abs.frontend.FrontendTest;
import abs.frontend.analyser.ErrorMessage;
import abs.frontend.analyser.SemanticErrorList;
import abs.frontend.ast.Model;

public class TypeCheckerTest extends FrontendTest {

	 // POSITIVE TESTS
   @Test
   public void stdLibFunctions() {
       assertNoTypeErrors(StandardLib.STDLIB_FUNCTIONS_STRING);
   }
	
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
        assertNoTypeErrors("interface I { Unit m(); } class C implements I { Unit m() { } }");
    }
    
    @Test
    public void methodNoReturnOk() {
        assertNoTypeErrors("interface I { Unit m(); } class C implements I { Unit m() { Bool b = True; b = False; } }");
    }

    @Test
    public void methodReturnOk() {
        assertNoTypeErrors("interface I { Unit m(); } class C implements I { Unit m() { Bool b = True; return Unit; } }");
    }

    @Test
    public void methodParameterOk() {
        assertNoTypeErrors("interface I { Bool m(Bool b);} " +
        		"class C implements I { Bool m(Bool b) { return b; } }");
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

    @Test
    public void syncCallMethodThis() {
        assertNoTypeErrors("interface I { Unit m(); } " +
        		"class C implements I { Unit m() { this.m(); } }");
    }
    
    @Test
    public void syncCallMethodThis2() {
        assertNoTypeErrors("interface I { Unit m(); } interface J {}" +
                "class C implements J,I { Unit m() { this.m(); } }");
    }
    
    @Test
    public void syncCallMethodThis3() {
        assertNoTypeErrors("interface I { Bool m(); } " +
                "class C implements I { Bool m() { Bool b = True; b = this.m(); return b; } }");
    }
    
    @Test
    public void syncCallMethodIntf() {
        assertNoTypeErrors("interface I { Unit m(); } {I i; i.m(); }");
    }
    
    @Test
    public void asyncCallMethodIntf() {
        assertNoTypeErrors("interface I { Unit m(); } {I i; i!m(); }");
    }

    @Test
    public void fnAppTypeArgs() {
        assertNoTypeErrors("def A f<A>(A a) = a; { Bool b = True; b = f(b); }");
    }

    @Test
    public void fnAppTypeArgs2() {
   	 assertNoTypeErrors(StandardLib.STDLIB_FUNCTIONS_STRING+
   			 "def B optValue<B>(Opt<B> val) = fromSome(val);");
    }
    
    @Test
    public void fnAppTypeArgs3() {
   	 assertNoTypeErrors(StandardLib.STDLIB_FUNCTIONS_STRING+
   			 "def List<B> tail2<B>(List<B> list) = tail(list) ; ");
    }

    @Test
    public void fnAppTypeArgs4() {
   	 assertNoTypeErrors(StandardLib.STDLIB_FUNCTIONS_STRING+
   			 "def B nth<B>(List<B> list, Int n) = nth(tail(list), n-1) ; ");
    }
    
    @Test
    public void constructorTypeArgs() {
        assertNoTypeErrors("{ Opt<Bool> o = Some(True); }");
    }
    
    @Test
    public void constructorTypeArgs2() {
        assertNoTypeErrors("data Foo<A> = Bar(A,A); { Foo<A> o = Bar(True,True); }");
    }
    
    @Test
    public void constructorTypeArgs3() {
        assertNoTypeErrors("data Foo<A,B> = Bar(A,B); { Foo<A,B> o = Bar(True,5); }");
    }

    @Test
    public void constructorTypeArgs4() {
        assertNoTypeErrors("{ Either<A,B> o = Left(5); }");
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
    public void functionDuplicateParams() {
        assertTypeErrors("def Bool f(Bool b, Int b) = True; "); 
    }

    @Test
    public void functionNestedErrors() {
        assertTypeErrors("def X f(Bool b) = True; "); 
        assertTypeErrors("def Bool f(X b) = True; "); 
        assertTypeErrors("def Bool f(Bool b) = Foo(); "); 
    }

    @Test
    public void functionWrongReturnType() {
        assertTypeErrors("def Bool f() = 5; "); 
    }
    
    @Test
    public void dataTypeDuplicateConstructors() {
        assertTypeErrors("data Foo = Bar | Bar ;"); 
    }

    @Test
    public void dataTypeNestedErrors() {
        assertTypeErrors("data Foo = Bar(X) ;"); 
    }

    @Test
    public void interfaceDuplicateMethods() {
        assertTypeErrors("interface I { Unit m(); Unit m(); }"); 
    }

    @Test
    public void methodDuplicateParams() {
        assertTypeErrors("interface I { Unit m(Bool b, Int b); }"); 
    }
    
    @Test
    public void interfaceWrongExtend() {
        assertTypeErrors("interface I extends Bool {} "); 
    }

    @Test
    public void interfaceMethodOverride() {
        assertTypeErrors("interface I { Unit m(); } interface J extends I { Unit m(); }"); 
    }

    @Test
    public void interfaceNestedError() {
        assertTypeErrors("interface I { X m(); }"); 
    }

    @Test
    public void interfaceNestedParamError() {
        assertTypeErrors("interface I { Unit m(X x); }"); 
    }
    
    @Test
    public void interfaceDupParams() {
        assertTypeErrors("interface I { Unit m(Bool b, Int b); }"); 
    }

    @Test
    public void classMethodOverride() {
        assertTypeErrors("interface I { } class C implements I { Unit m() { } }"); 
    }

    @Test
    public void classMethodWrongNumParams() {
        assertTypeErrors("interface I { Unit m(); } class C implements I { Unit m(Bool b) { } }"); 
    }

    @Test
    public void classMethodWrongParamType() {
        assertTypeErrors("interface I { Unit m(Bool b); } class C implements I { Unit m(Int i) { } }"); 
    }
    
    @Test
    public void classDuplicateFields() {
        assertTypeErrors("class C { Bool f; Int f;}"); 
    }

    @Test
    public void classFieldError() {
        assertTypeErrors("class C { X f; }"); 
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
    public void newError() {
        assertTypeErrors("interface I { } { I i; i = new I(); }"); 
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
    public void fnAppMissingDef() {
        assertTypeErrors("def Bool f() = x(); "); 
    }
    
    @Test
    public void fnAppWrongArgNum() {
        assertTypeErrors("def Bool f() = f(True); "); 
    }

    @Test
    public void fnAppWrongArgType() {
        assertTypeErrors("def Bool f(Bool b) = f(5); "); 
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
    public void syncCallWrongTarget() {
        assertTypeErrors("class C { Unit m(Bool b) { b.m();  } }");
    }

    @Test
    public void syncCallNoMethodThis() {
        assertTypeErrors("class C { Unit m() { this.n(); } }");
    }

    @Test
    public void syncCallNoMethodIntf() {
        assertTypeErrors("interface I {} { I i; i.m(); }");
    }

    @Test
    public void syncCallWrongArgNum() {
        assertTypeErrors("interface I { Unit m(); } { I i; i.m(True); }");
    }

    @Test
    public void syncCallWrongArgType() {
        assertTypeErrors("interface I { Unit m(Int i); } { I i; i.m(True); }");
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
    
    @Test
    public void constructorTypeArgsError() {
        assertTypeErrors("data Foo<A> = Bar(A,A); { Foo<A> o = Bar(True,5); }");
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
       if (!l.isEmpty()) {
           msg = l.getFirst().getMsgWithHint(absCode);
       }
       assertEquals(msg,expectErrors, !l.isEmpty());
   }


}
