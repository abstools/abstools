package abs.backend.common;

import org.junit.Test;
import org.junit.runner.RunWith;

import abs.backend.BackendTestDriver;
import org.junit.runners.Parameterized;


@RunWith(Parameterized.class)
public class PrimitiveTypes extends SemanticTests {

    public PrimitiveTypes(BackendTestDriver d) {
        super(d);
    }

    
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
    public void intAdd() {
       assertEvalTrue("{ Bool testresult = 5 + 5 == 10;  }"); 
    }
    
    @Test
    public void intSub() {
       assertEvalTrue("{ Bool testresult = 10 - 5 == 5;  }"); 
    }

    @Test
    public void intDiv() {
       assertEvalTrue("{ Bool testresult = 10 / 5 == 2;  }"); 
    }

    @Test
    public void intMult() {
       assertEvalTrue("{ Bool testresult = 2 * 5 == 10;  }"); 
    }

    @Test
    public void intMod() {
       assertEvalTrue("{ Bool testresult = 10 % 3 == 1;  }"); 
    }

    @Test
    public void intGt() {
       assertEvalTrue("{ Bool testresult = 10 > 5;  }"); 
    }

    @Test
    public void intLt() {
       assertEvalTrue("{ Bool testresult = 5 < 10;  }"); 
    }

    @Test
    public void intLtEq() {
       assertEvalTrue("{ Bool testresult = 5 <= 10;  }"); 
    }

    @Test
    public void intLtEq2() {
       assertEvalTrue("{ Bool testresult = 5 <= 5;  }"); 
    }
    

    @Test
    public void intGtEq() {
       assertEvalTrue("{ Bool testresult = 10 >= 5;  }"); 
    }

    @Test
    public void intGtEq2() {
       assertEvalTrue("{ Bool testresult = 5 >= 5;  }"); 
    }
    

    @Test
    public void stringEq() {
       assertEvalTrue("{ Bool testresult = \"xx\" == \"xx\";  }"); 
    }

    @Test
    public void stringNeq() {
       assertEvalTrue("{ Bool testresult = \"xx\" != \"yx\";  }"); 
    }

    @Test
    public void stringConcat() {
       assertEvalTrue("{ Bool testresult = \"x\" + \"x\" == \"xx\";  }"); 
    }

}
