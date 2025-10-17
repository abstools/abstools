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
    public void boolTest() throws Exception {
        assertEvalTrue("{ Bool testresult = True;  }");
    }

    @Test
    public void boolAnd() throws Exception {
        assertEvalTrue("{ Bool testresult = False; testresult = True && True;  }");
    }

    @Test
    public void boolOr() throws Exception {
        assertEvalTrue("{ Bool testresult = True || False;  }");
    }

    @Test
    public void boolNegBindsOverOr() throws Exception { assertEvalTrue("{ Bool testresult = !False || True; }"); }

    @Test
    public void intMinus() throws Exception {
        assertEvalTrue("{ Int x = -1; x = -x; Bool testresult = x == 1; }");
    }

    @Test
    public void intAdd() throws Exception {
        assertEvalTrue("{ Bool testresult = 5 + 5 == 10;  }");
    }

    @Test
    public void intAddLong() throws Exception {
        assertEvalTrue("{ Bool testresult = 92233720368547758070 + 9223372036854775807 == 101457092405402533877;  }");
    }

    @Test
    public void intSub() throws Exception {
        assertEvalTrue("{ Bool testresult = 10 - 5 == 5;  }");
    }

    @Test
    public void intSubLong() throws Exception {
        assertEvalTrue("{ Bool testresult = 92233720368547758070 - 9223372036854775807 == 83010348331692982263;  }");
    }

    @Test
    public void intDiv() throws Exception {
        assertEvalTrue("{ Int x = truncate(10/4); Bool testresult = x == 2;  }");
    }

    @Test
    public void intMult() throws Exception {
        assertEvalTrue("{ Bool testresult = 2 * 5 == 10;  }");
    }

    @Test
    public void intMultLong() throws Exception {
        assertEvalTrue("{ Bool testresult = 92233720368547758070 * 9223372036854775807 == 850705917302346158473969077842325012490;  }");
    }

    @Test
    public void intMod() throws Exception {
        assertEvalTrue("{ Bool testresult = 10 % 3 == 1;  }");
    }

    @Test
    public void intGt() throws Exception {
        assertEvalTrue("{ Bool testresult = 10 > 5;  }");
    }

    @Test
    public void intLt() throws Exception {
        assertEvalTrue("{ Bool testresult = 5 < 10;  }");
    }

    @Test
    public void intLtLong() throws Exception {
        // 2^63, i.e., Long.MAX_VALUE + 1
        assertEvalTrue("{ Bool testresult = 5 < 9223372036854775808;  }");
    }

    @Test
    public void intLtEq() throws Exception {
        assertEvalTrue("{ Bool testresult = 5 <= 10;  }");
    }

    @Test
    public void intLtEq2() throws Exception {
        assertEvalTrue("{ Bool testresult = 5 <= 5;  }");
    }

    @Test
    public void intGtEq() throws Exception {
        assertEvalTrue("{ Bool testresult = 10 >= 5;  }");
    }

    @Test
    public void intGtEq2() throws Exception {
        assertEvalTrue("{ Bool testresult = 5 >= 5;  }");
    }

    @Test
    public void intDoubleMinusFolding() throws Exception {
        // Test that constant folding does not result in literals like `--1`, which are invalid.
        // See https://github.com/abstools/abstools/issues/283
        assertEvalTrue("{ Int i =  -(0 - 1) + 1; Bool testresult = i == 2; }");
    }

    @Test
    public void ratMinus() throws Exception {
        assertEvalTrue("{ Rat x = -1/2; x = -x; Bool testresult = x == 1/2; }");
    }

    @Test
    public void ratAdd() throws Exception {
        assertEvalTrue("{ Bool testresult = 5/2 + 5/2 == 5;  }");
    }

    @Test
    public void ratSub() throws Exception {
        assertEvalTrue("{ Bool testresult = 10/3 - 1/3 == 3;  }");
    }

    @Test
    public void ratDiv() throws Exception {
        assertEvalTrue("{ Rat x = 10/4; Bool testresult = x == 5/2;  }");
    }

    @Test
    public void ratMult() throws Exception {
        assertEvalTrue("{ Bool testresult = (2/5) * (5/2) == 1;  }");
    }

    @Test
    public void ratMod() throws Exception {
        assertEvalTrue("{ Rat x = 5/3 % 1/2; Bool testresult = x == 1/3;  }");
    }

    @Test
    public void ratEq() throws Exception {
        assertEvalTrue("{ Bool testresult = 8/24 == 2/6 ;  }");
    }

    @Test
    public void ratGt() throws Exception {
        assertEvalTrue("{ Bool testresult = 5/3 > 4/3;  }");
    }

    @Test
    public void ratLt() throws Exception {
        assertEvalTrue("{ Bool testresult = 4/3 < 5/3;  }");
    }

    @Test
    public void ratLtEq() throws Exception {
        assertEvalTrue("{ Bool testresult = 4/3 <= 5/3;  }");
    }

    @Test
    public void ratLtEq2() throws Exception {
        assertEvalTrue("{ Bool testresult = 5/2 <= 5/2;  }");
    }

    @Test
    public void ratGtEq() throws Exception {
        assertEvalTrue("{ Bool testresult = 5/3 >= 4/3;  }");
    }

    @Test
    public void ratGtEq2() throws Exception {
        assertEvalTrue("{ Bool testresult = 5/3 >= 5/3;  }");
    }

    @Test
    public void floatMinus() throws Exception {
        assertEvalTrue("{ Float x = -1.0; x = -x; Bool testresult = x == 1.0; }");
    }

    @Test
    public void floatAdd() throws Exception {
        assertEvalTrue("{ Bool testresult = 5.0 + 5.0 == 10.0;  }");
    }

    @Test
    public void floatSub() throws Exception {
        assertEvalTrue("{ Bool testresult = 10.0 - 5.0 == 5.0;  }");
    }

    @Test
    public void floatDiv() throws Exception {
        assertEvalTrue("{ Float result = 10.0/4.0; Bool testresult = result == 2.5;  }");
    }

    @Test
    public void floatMult() throws Exception {
        assertEvalTrue("{ Bool testresult = 2.0 * 5.0 == 10.0;  }");
    }

    @Test
    public void floatMod() throws Exception {
        assertEvalTrue("{ Bool testresult = 10.0 % 3.0 == 1.0;  }");
    }

    @Test
    public void floatGt() throws Exception {
        assertEvalTrue("{ Bool testresult = 10.0 > 5.0;  }");
    }

    @Test
    public void floatLt() throws Exception {
        assertEvalTrue("{ Bool testresult = 5.0 < 10.0;  }");
    }

    @Test
    public void floatLtEq() throws Exception {
        assertEvalTrue("{ Bool testresult = 5.0 <= 10.0;  }");
    }

    @Test
    public void floatLtEq2() throws Exception {
        assertEvalTrue("{ Bool testresult = 5.0 <= 5.0;  }");
    }

    @Test
    public void floatGtEq() throws Exception {
        assertEvalTrue("{ Bool testresult = 10.0 >= 5.0;  }");
    }

    @Test
    public void floatGtEq2() throws Exception {
        assertEvalTrue("{ Bool testresult = 5.0 >= 5.0;  }");
    }

    @Test
    public void stringEq() throws Exception {
        assertEvalTrue("{ Bool testresult = \"xx\" == \"xx\";  }");
    }

    @Test
    public void stringNeq() throws Exception {
        assertEvalTrue("{ Bool testresult = \"xx\" != \"yx\";  }");
    }

    @Test
    public void templatestringEq() throws Exception {
        assertEvalTrue("{ Bool testresult = `xx` == `xx`; }");
    }

    @Test
    public void templatestringNeq() throws Exception {
        assertEvalTrue("{ Bool testresult = `xx` != `yx`; }");
    }

    @Test
    public void stringTemplatestringEq() throws Exception {
        assertEvalTrue("{ Bool testresult = `x\\`x` == \"x`x\"; }");
    }

    @Test
    public void stringTemplatestringNeq() throws Exception {
        assertEvalTrue("{ Bool testresult = `xx` != \"yx\"; }");
    }

    @Test
    public void stringConcat() throws Exception {
        assertEvalTrue("{ Bool testresult = \"x\" + \"x\" == \"xx\";  }");
    }

    @Test
    public void stringNotEq() throws Exception {
        assertEvalTrue("{ Bool testresult = \"\" != \" \"; }");
    }

   @Test
    public void intRatGtEq() throws Exception {
        assertEvalTrue("{ Bool testresult = 101/100 > 1;  }");
    }

    @Test
    public void intRatLtEq() throws Exception {
        assertEvalTrue("{ Bool testresult = 9999999/10000000 < 1;  }");
    }

    @Test
    public void intRatCmp() throws Exception {
        assertEvalTrue("{ Bool testresult = 10000000/10000000 == 1;  }");
    }

    @Test
    public void intRatCmp2() throws Exception {
        assertEvalTrue("{ Bool testresult = 10000000/10000000 != 1; testresult= ! testresult; }");
    }

    @Test
    public void bug131() throws Exception {
        // Test that non-trivial arithmetic expressions are properly bracketized in generated code
        assertEvalTrue("def Int a() = 5; { Bool testresult = 100-(a() + 2) == 93; }");
    }

    @Test
    public void wrappedIntRatCmp() throws Exception {
        assertEvalTrue("{ Bool testresult = Time(2) == Time(2/1); }");
    }

    @Test
    public void boolToString() throws Exception {
	    assertEvalTrue("{ Bool testresult = toString(True) == \"True\"; }");
	    assertEvalTrue("{ Bool testresult = toString(False) == \"False\"; }");
    }

    @Test
    public void intToString() throws Exception {
	    assertEvalTrue("{ Bool testresult = toString(5) == \"5\"; }");
    }

    @Test
    public void ratToString() throws Exception {
	    assertEvalTrue("{ Bool testresult = toString(5/2) == \"5/2\"; }");
    }

    @Test
    public void floatToString() throws Exception {
        assertEvalTrue("{ Bool testresult = toString(2.5) == \"2.5\"; }");
    }

    @Test
    public void ratToFloat() throws Exception {
        // Note that this will not be true for all numbers / backends.  E.g.,
        // this test fails for 13.53 == 1353/100 on the Java backend, but not
        // the Erlang backend
        assertEvalTrue("{ Bool testresult = rat(13.5) == 27/2; }");
    }

     @Test
     public void stringToString() throws Exception {
	assertEvalTrue("{ Bool testresult = toString(\"Hello\") == \"Hello\"; }");
    }

    @Test
    public void datatypeToString() throws Exception {
        assertEvalTrue("""
            data Person = Person(String, Int);
            {
                Bool testresult = toString(Person("Joe", 20)) == `Person("Joe",20)`;
            }
            """);
    }
}
