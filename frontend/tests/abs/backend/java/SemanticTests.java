package abs.backend.java;

import org.junit.Test;

public class SemanticTests extends JavaBackendTest {

    @Test
    public void boolTest() {
       assertEvalTrue("{ Bool testresult = True;  }"); 
    }

    @Test
    public void boolAnd() {
       assertEvalTrue("{ Bool testresult = False; testresult = True && True;  }"); 
    }

    @Test
    public void boolOr() {
       assertEvalTrue("{ Bool testresult = True || False;  }"); 
    }

    @Test
    public void boolIfThen() {
       assertEvalTrue("{ Bool testresult = False; if (True) testresult = True;  }"); 
    }

    @Test
    public void boolIfFalse() {
       assertEvalTrue("{ Bool testresult = False; if (False) ; else testresult = True;  }"); 
    }

    @Test
    public void classDecl() {
       assertEvalTrue("class C { } { Bool testresult = True; }"); 
    }

    @Test
    public void intfClassDecl() {
       assertEvalTrue("interface I { } class C implements I { } { Bool testresult = True; I i; }"); 
    }

    
}
