package abs.backend.common;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;

import abs.backend.BackendTestDriver;

@RunWith(Parameterized.class)
public class ObjectTests extends SemanticTests {

    public ObjectTests(BackendTestDriver d) {
        super(d);
    }

    @Test
    public void classDecl() {
       assertEvalTrue("class C { } { Bool testresult = True; }"); 
    }

    private static String EMPTY_CLASS_C = "interface I { } class C implements I { }" ; 
    
    @Test
    public void intfClassDecl() {
       assertEvalTrue(EMPTY_CLASS_C+"{ Bool testresult = True; I i; }"); 
    }
    
    @Test
    public void newExp() {
       assertEvalTrue(EMPTY_CLASS_C+" { Bool testresult = True; I i; i = new C();}"); 
    }
    
    @Test
    public void refEq() {
       assertEvalTrue(EMPTY_CLASS_C+" { Bool testresult = True; I i; i = new C(); testresult = i == i;}"); 
    }

    
    private static String CLASS_WITH_METHOD = "interface I { Bool m(); } class C implements I { Bool m() { return True; } }" ; 

    @Test
    public void methodCall() {
       assertEvalTrue(CLASS_WITH_METHOD+" { Bool testresult = True; I i; i = new C(); testresult = i.m();}"); 
    }

    private static String INTERFACE_I = "interface I { Bool m(); }";
    private static String CLASS_WITH_FIELD = INTERFACE_I + " class C(Bool f) implements I { Bool m() { return this.f; } }" ;

    private static String CALL_M = " { Bool testresult = True; I i; i = new C(True); testresult = i.m();}";
    
    @Test
    public void fieldAccess() {
       assertEvalTrue(CLASS_WITH_FIELD+CALL_M); 
    }
    
    private static String CLASS_WITH_ASSIGN = INTERFACE_I+"class C(Bool f) implements I { Bool x = False; Bool m() { this.x = this.f; return this.x; } }" ;

    @Test
    public void fieldAssign() {
       assertEvalTrue(CLASS_WITH_ASSIGN+CALL_M); 
    }

    private static String CLASS_WITH_INITBLOCK = INTERFACE_I+"class C(Bool f) implements I { Bool x = False; { x = True; } Bool m() { return this.x; } }" ;
    
    @Test
    public void initBlock() {
       assertEvalTrue(CLASS_WITH_INITBLOCK+CALL_M); 
    }

    @Test
    public void classParamReadInFieldAssign() {
       assertEvalTrue(INTERFACE_I+" class C(Bool f) implements I { Bool x = f; Bool m() { return this.x; } }"+CALL_M); 
    }

}
