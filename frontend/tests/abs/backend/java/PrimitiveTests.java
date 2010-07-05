package abs.backend.java;

import org.junit.Test;

public class PrimitiveTests extends JavaBackendTest {
    @Test
    public void testNullLit() {
        assertValid("interface I { } { I i; i = null; }");
    }

    @Test
    public void testBoolLit() {
        assertValid("data Bool {  } { Bool b; b = True; b = False; }");
    }

    @Test
    public void testBoolNeg() {
        assertValid("data Bool {  } { Bool b; b = ~True; }");
    }
    
    @Test
    public void testBoolAnd() {
        assertValid("data Bool {  } { Bool b; b = True && False; }");
    }

    @Test
    public void testBoolOr() {
        assertValid("data Bool {  } { Bool b; b = True || False; }");
    }

    @Test
    public void testBoolEq() {
        assertValid("data Bool {  } { Bool b; b = True == False; }");
    }

    @Test
    public void testBoolNotEq() {
        assertValid("data Bool {  } { Bool b; b = True != False; }");
    }
    
    
    @Test
    public void testIntLit() {
        assertValid("data Int { } { Int i; i = 5; }");
    }

    @Test
    public void testNegativeIntLit() {
        assertValid("data Int { } { Int i; i = -7; }");
    }

    @Test
    public void testLongIntLit() {
        assertValid("data Int { } { Int i; i = 534023840238420394820394823094; }");
    }
    
    @Test
    public void testIntAddOps() {
        assertValid("data Int { } { Int i; i = 5 + -7; }");
    }
    
    @Test
    public void testIntSubtractOps() {
        assertValid("data Int { } { Int i; i = 7 - 5; }");
    }

    @Test
    public void testIntMultiplyOps() {
        assertValid("data Int { } { Int i; i = 7 * 5; }");
    }

    @Test
    public void testIntDivideOps() {
        assertValid("data Int { } { Int i; i = 7 / 5; }");
    }

    @Test
    public void testIntModOps() {
        assertValid("data Int { } { Int i; i = 7 % 5; }");
    }
    
    @Test
    public void testIntCompareOps() {
        assertValid("data Int { } data Bool { } { Bool b; b = 7 == 5; }");
        assertValid("data Int { } data Bool { } { Bool b; b = 7 != 5; }");
        assertValid("data Int { } data Bool { } { Bool b; b = 7 > 5; }");
        assertValid("data Int { } data Bool { } { Bool b; b = 7 < 5; }");
        assertValid("data Int { } data Bool { } { Bool b; b = 7 >= 5; }");
        assertValid("data Int { } data Bool { } { Bool b; b = 7 <= 5; }");
    }

    
    @Test
    public void testStringLit() {
        assertValid("data String { } { String s; s = \"Test\"; }");
    }
    
    @Test
    public void testStringCompareOps() {
        assertValid("data String { } data Bool {} { Bool b; b = \"Test\" == \"Test\"; }");
        assertValid("data String { } data Bool {} { Bool b; b = \"Test\" != \"Test\"; }");
    }

}
