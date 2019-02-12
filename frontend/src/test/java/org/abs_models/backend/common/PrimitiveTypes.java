/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.common;

import org.abs_models.backend.BackendTestDriver;
import org.junit.Test;
import org.junit.runner.RunWith;
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
    public void intMinus() {
        assertEvalTrue("{ Int x = -1; x = -x; Bool testresult = x == 1; }");
    }

    @Test
    public void intAdd() {
        assertEvalTrue("{ Bool testresult = 5 + 5 == 10;  }");
    }

    @Test
    public void intAddLong() {
        assertEvalTrue("{ Bool testresult = 92233720368547758070 + 9223372036854775807 == 101457092405402533877;  }");
    }

    @Test
    public void intSub() {
        assertEvalTrue("{ Bool testresult = 10 - 5 == 5;  }");
    }

    @Test
    public void intSubLong() {
        assertEvalTrue("{ Bool testresult = 92233720368547758070 - 9223372036854775807 == 83010348331692982263;  }");
    }

    @Test
    public void intDiv() {
        assertEvalTrue("{ Int x = truncate(10/4); Bool testresult = x == 2;  }");
    }

    @Test
    public void intMult() {
        assertEvalTrue("{ Bool testresult = 2 * 5 == 10;  }");
    }

    @Test
    public void intMultLong() {
        assertEvalTrue("{ Bool testresult = 92233720368547758070 * 9223372036854775807 == 850705917302346158473969077842325012490;  }");
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
    public void intLtLong() {
        // 2^63, i.e., Long.MAX_VALUE + 1
        assertEvalTrue("{ Bool testresult = 5 < 9223372036854775808;  }");
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
    public void ratMinus() {
        assertEvalTrue("{ Rat x = -1/2; x = -x; Bool testresult = x == 1/2; }");
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
    public void floatMinus() {
        assertEvalTrue("{ Float x = -1.0; x = -x; Bool testresult = x == 1.0; }");
    }

    @Test
    public void floatAdd() {
        assertEvalTrue("{ Bool testresult = 5.0 + 5.0 == 10.0;  }");
    }

    @Test
    public void floatSub() {
        assertEvalTrue("{ Bool testresult = 10.0 - 5.0 == 5.0;  }");
    }

    @Test
    public void floatDiv() {
        assertEvalTrue("{ Float result = 10.0/4.0; Bool testresult = result == 2.5;  }");
    }

    @Test
    public void floatMult() {
        assertEvalTrue("{ Bool testresult = 2.0 * 5.0 == 10.0;  }");
    }

    @Test
    public void floatMod() {
        assertEvalTrue("{ Bool testresult = 10.0 % 3.0 == 1.0;  }");
    }

    @Test
    public void floatGt() {
        assertEvalTrue("{ Bool testresult = 10.0 > 5.0;  }");
    }

    @Test
    public void floatLt() {
        assertEvalTrue("{ Bool testresult = 5.0 < 10.0;  }");
    }

    @Test
    public void floatLtEq() {
        assertEvalTrue("{ Bool testresult = 5.0 <= 10.0;  }");
    }

    @Test
    public void floatLtEq2() {
        assertEvalTrue("{ Bool testresult = 5.0 <= 5.0;  }");
    }

    @Test
    public void floatGtEq() {
        assertEvalTrue("{ Bool testresult = 10.0 >= 5.0;  }");
    }

    @Test
    public void floatGtEq2() {
        assertEvalTrue("{ Bool testresult = 5.0 >= 5.0;  }");
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
    public void templatestringEq() {
        assertEvalTrue("{ Bool testresult = `xx` == `xx`; }");
    }

    @Test
    public void templatestringNeq() {
        assertEvalTrue("{ Bool testresult = `xx` != `yx`; }");
    }

    @Test
    public void stringTemplatestringEq() {
        assertEvalTrue("{ Bool testresult = `xx` == \"xx\"; }");
    }

    @Test
    public void stringTemplatestringNeq() {
        assertEvalTrue("{ Bool testresult = `xx` != \"yx\"; }");
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
    public void bug131() {
        // Test that non-trivial arithmetic expressions are properly bracketized in generated code
        assertEvalTrue("def Int a() = 5; { Bool testresult = 100-(a() + 2) == 93; }");
    }

    @Test
    public void wrappedIntRatCmp() {
        assertEvalTrue("{ Bool testresult = Time(2) == Time(2/1); }");
    }

    @Test
    public void intToString() {
	assertEvalTrue("{ Bool testresult = toString(5) == \"5\"; }");
    }

    @Test
    public void ratToString() {
	assertEvalTrue("{ Bool testresult = toString(5/2) == \"5/2\"; }");
    }

    @Test
    public void floatToString() {
        assertEvalTrue("{ Bool testresult = toString(2.5) == \"2.5\"; }");
    }

    @Test
    public void stringToString() {
	assertEvalTrue("{ Bool testresult = toString(\"Hello\") == \"Hello\"; }");
    }
}
