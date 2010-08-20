package abs.backend.common;

import java.util.Arrays;
import java.util.Collection;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

import abs.backend.BackendTestDriver;
import abs.backend.java.JavaTestDriver;
import abs.backend.maude.MaudeTestDriver;

@RunWith(Parameterized.class)
public class SemanticTests {
    
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
    
    
    private void assertEvalTrue(String absCode) {
        driver.assertEvalTrue(absCode);
    }


    private BackendTestDriver driver;

    public SemanticTests(BackendTestDriver d) {
        driver = d;
    }
    
    @Parameters
    public static Collection<?> data() {
      Object[][] data = new Object[][] { { new JavaTestDriver() }, { new MaudeTestDriver() } };
      return Arrays.asList(data);
    }

}
