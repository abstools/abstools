package abs.backend.java;

import org.junit.Test;

public class JavaPrimitiveTests extends JavaBackendTest {
    @Test
    public void testNullLit() {
        assertValid("interface I { } { I i; i = null; }");
    }

    @Test
    public void testBoolLit() {
        assertValidStdLib(" { Bool b = True; b = False; }");
    }

    @Test
    public void testBoolNeg() {
        assertValidStdLib(" { Bool b = ~True; }");
    }

    @Test
    public void testBoolAnd() {
        assertValidStdLib(" { Bool b = True && False; }");
    }

    @Test
    public void testBoolOr() {
        assertValidStdLib(" { Bool b = True || False; }");
    }

    @Test
    public void testBoolEq() {
        assertValidStdLib(" { Bool b = True == False; }");
    }

    @Test
    public void testBoolNotEq() {
        assertValidStdLib(" { Bool b = True != False; }");
    }

    @Test
    public void testIntLit() {
        assertValidStdLib(" { Int i = 5; }");
    }

    @Test
    public void testNegativeIntLit() {
        assertValidStdLib(" { Int i = -7; }");
    }

    @Test
    public void testLongIntLit() {
        assertValidStdLib(" { Int i = 534023840238420394820394823094; }");
    }

    @Test
    public void testIntAddOps() {
        assertValidStdLib(" { Int i = 5 + -7; }");
    }

    @Test
    public void testIntSubtractOps() {
        assertValidStdLib(" { Int i = 7 - 5; }");
    }

    @Test
    public void testIntMultiplyOps() {
        assertValidStdLib(" { Int i = 7 * 5; }");
    }

    @Test
    public void testIntDivideOps() {
        assertValidStdLib(" { Int i = 7 / 5; }");
    }

    @Test
    public void testIntModOps() {
        assertValidStdLib(" { Int i = 7 % 5; }");
    }

    @Test
    public void testIntCompareOps() {
        assertValidStdLib("{ Bool b = 7 == 5; }");
        assertValidStdLib("{ Bool b = 7 != 5; }");
        assertValidStdLib("{ Bool b = 7 > 5; }");
        assertValidStdLib("{ Bool b = 7 < 5; }");
        assertValidStdLib("{ Bool b = 7 >= 5; }");
        assertValidStdLib("{ Bool b = 7 <= 5; }");
    }

    @Test
    public void testStringLit() {
        assertValidStdLib("{ String s = \"Test\"; }");
    }

    @Test
    public void testStringCompareOps() {
        assertValidStdLib("{ Bool b = \"Test\" == \"Test\"; }");
        assertValidStdLib("{ Bool b = \"Test\" != \"Test\"; }");
    }

}
