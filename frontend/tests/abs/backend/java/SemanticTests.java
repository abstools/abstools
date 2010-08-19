package abs.backend.java;

import org.junit.Test;

public class SemanticTests extends JavaBackendTest {

    @Test
    public void boolTest() {
       assertEvalTrue("{ Bool testresult = True;  }"); 
    }

    @Test
    public void boolTestAnd() {
       assertEvalTrue("{ Bool testresult = False; testresult = True && True;  }"); 
    }

    @Test
    public void boolTestOr() {
       assertEvalTrue("{ Bool testresult = True || False;  }"); 
    }
}
