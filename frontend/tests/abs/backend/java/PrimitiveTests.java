package abs.backend.java;

import org.junit.Test;

public class PrimitiveTests extends JavaBackendTest {
    @Test
    public void testNullLit() {
        assertValid("interface I { } { I i; i = null; }");
    }
    
    @Test
    public void testBoolLit() {
        assertValidStdLib(" { Bool b; b = True; b = False; }");
    }

    @Test
    public void testBoolNeg() {
        assertValidStdLib(" { Bool b; b = ~True; }");
    }
    
    @Test
    public void testBoolAnd() {
        assertValidStdLib(" { Bool b; b = True && False; }");
    }

    @Test
    public void testBoolOr() {
        assertValidStdLib(" { Bool b; b = True || False; }");
    }

    @Test
    public void testBoolEq() {
        assertValidStdLib(" { Bool b; b = True == False; }");
    }

    @Test
    public void testBoolNotEq() {
        assertValidStdLib(" { Bool b; b = True != False; }");
    }
    
    
    @Test
    public void testIntLit() {
        assertValidStdLib(" { Int i; i = 5; }");
    }

    @Test
    public void testNegativeIntLit() {
        assertValidStdLib(" { Int i; i = -7; }");
    }

    @Test
    public void testLongIntLit() {
        assertValidStdLib(" { Int i; i = 534023840238420394820394823094; }");
    }
    
    @Test
    public void testIntAddOps() {
        assertValidStdLib(" { Int i; i = 5 + -7; }");
    }
    
    @Test
    public void testIntSubtractOps() {
        assertValidStdLib(" { Int i; i = 7 - 5; }");
    }

    @Test
    public void testIntMultiplyOps() {
        assertValidStdLib(" { Int i; i = 7 * 5; }");
    }

    @Test
    public void testIntDivideOps() {
        assertValidStdLib(" { Int i; i = 7 / 5; }");
    }

    @Test
    public void testIntModOps() {
        assertValidStdLib(" { Int i; i = 7 % 5; }");
    }
    
    @Test
    public void testIntCompareOps() {
        assertValidStdLib("{ Bool b; b = 7 == 5; }");
        assertValidStdLib("{ Bool b; b = 7 != 5; }");
        assertValidStdLib("{ Bool b; b = 7 > 5; }");
        assertValidStdLib("{ Bool b; b = 7 < 5; }");
        assertValidStdLib("{ Bool b; b = 7 >= 5; }");
        assertValidStdLib("{ Bool b; b = 7 <= 5; }");
    }

    
    @Test
    public void testStringLit() {
        assertValidStdLib("{ String s; s = \"Test\"; }");
    }
    
    @Test
    public void testStringCompareOps() {
        assertValidStdLib("{ Bool b; b = \"Test\" == \"Test\"; }");
        assertValidStdLib("{ Bool b; b = \"Test\" != \"Test\"; }");
    }

}
