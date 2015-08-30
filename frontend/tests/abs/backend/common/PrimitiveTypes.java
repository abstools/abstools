/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
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
        assertEvalTrue("{ Int x = 10/4; Bool testresult = x == 2;  }");
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
    public void ratAdd() {
        assertEvalTrue("{ Bool testresult = 5/2 + 5/2 == 5;  }");
    }

    @Test
    public void ratSub() {
        assertEvalTrue("{ Bool testresult = 10/3 - 1/3 == 3;  }");
    }

    @Test
    public void ratDiv() {
        assertEvalTrue("{ Rat x = 10/4; Bool testresult = x == 5/2;  }");
    }

    @Test
    public void ratMult() {
        assertEvalTrue("{ Bool testresult = (2/5) * (5/2) == 1;  }");
    }

    @Test
    public void ratMod() {
        assertEvalTrue("{ Rat x = 5/3 % 1/2; Bool testresult = x == 1/3;  }");
    }

    @Test
    public void ratEq() {
        assertEvalTrue("{ Bool testresult = 8/24 == 2/6 ;  }");
    }

    @Test
    public void ratGt() {
        assertEvalTrue("{ Bool testresult = 5/3 > 4/3;  }");
    }

    @Test
    public void ratLt() {
        assertEvalTrue("{ Bool testresult = 4/3 < 5/3;  }");
    }

    @Test
    public void ratLtEq() {
        assertEvalTrue("{ Bool testresult = 4/3 <= 5/3;  }");
    }

    @Test
    public void ratLtEq2() {
        assertEvalTrue("{ Bool testresult = 5/2 <= 5/2;  }");
    }

    @Test
    public void ratGtEq() {
        assertEvalTrue("{ Bool testresult = 5/3 >= 4/3;  }");
    }

    @Test
    public void ratGtEq2() {
        assertEvalTrue("{ Bool testresult = 5/3 >= 5/3;  }");
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

    @Test
    public void stringNotEq() {
        assertEvalTrue("{ Bool testresult = \"\" != \" \"; }");
    }

    @Test
    public void intRatParameter() {
        assertEvalTrue("def Int c(Int a)=a/2; { Int a =c(5/2); Bool testresult = a ==1; }");
    }

    @Test
    public void intRatLet() {
        assertEvalTrue("{  Bool testresult = let (Int x) = 5/2 in x == 2; }");
    }

    @Test
    public void intRatGtEq() {
        assertEvalTrue("{ Bool testresult = 101/100 > 1;  }");
    }

    @Test
    public void intRatLtEq() {
        assertEvalTrue("{ Bool testresult = 9999999/10000000 < 1;  }");
    }

    @Test
    public void intRatCmp() {
        assertEvalTrue("{ Bool testresult = 10000000/10000000 == 1;  }");
    }

    @Test
    public void intRatCmp2() {
        assertEvalTrue("{ Bool testresult = 10000000/10000000 != 1; testresult= ! testresult; }");
    }

    @Test
    public void divByZero() throws Exception {
        assertEvalFails("{Bool testresult = 1/0 != 0;}");
    }
}
