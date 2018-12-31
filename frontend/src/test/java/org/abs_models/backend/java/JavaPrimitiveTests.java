/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.java;

import org.abs_models.backend.java.codegeneration.JavaCodeGenerationException;
import org.junit.Test;

public class JavaPrimitiveTests extends JavaBackendTest {
    @Test
    public void testNullLit() throws Exception {
        assertValid("interface I { } { I i; i = null; }");
    }

    @Test
    public void testBoolLit() throws Exception {
        assertValid(" { Bool b = True; b = False; }");
    }

    @Test
    public void testBoolNeg() throws Exception {
        assertValid(" { Bool b = !True; }");
    }

    @Test
    public void testBoolAnd() throws Exception {
        assertValid(" { Bool b = True && False; }");
    }

    @Test
    public void testBoolOr() throws Exception {
        assertValid(" { Bool b = True || False; }");
    }

    @Test
    public void testBoolEq() throws Exception {
        assertValid(" { Bool b = True == False; }");
    }

    @Test
    public void testBoolNotEq() throws Exception {
        assertValid(" { Bool b = True != False; }");
    }

    @Test
    public void testIntLit() throws Exception {
        assertValid(" { Int i = 5; }");
    }

    @Test
    public void testNegativeIntLit() throws Exception {
        assertValid(" { Int i = -7; }");
    }

    @Test
    public void testLongIntLit() throws Exception {
        assertValid(" { Int i = 534023840238420394820394823094; }");
    }

    @Test
    public void testIntAddOps() throws Exception {
        assertValid(" { Int i = 5 + -7; }");
    }

    @Test
    public void testIntSubtractOps() throws Exception {
        assertValid(" { Int i = 7 - 5; }");
    }

    @Test
    public void testIntMultiplyOps() throws Exception {
        assertValid(" { Int i = 7 * 5; }");
    }

    @Test
    public void testIntDivideOps() throws Exception {
        assertValid(" { Int i = truncate(7 / 5); }");
    }

    @Test
    public void testIntModOps() throws Exception {
        assertValid(" { Int i = 7 % 5; }");
    }

    @Test
    public void testIntCompareOps() throws Exception {
        assertValid("{ Bool b = 7 == 5; }");
        assertValid("{ Bool b = 7 != 5; }");
        assertValid("{ Bool b = 7 > 5; }");
        assertValid("{ Bool b = 7 < 5; }");
        assertValid("{ Bool b = 7 >= 5; }");
        assertValid("{ Bool b = 7 <= 5; }");
    }

    @Test
    public void testStringLit() throws Exception {
        assertValid("{ String s = \"Test\"; }");
    }
    
    @Test
    public void testStringLitEscaped() throws Exception {
        assertValid("{ String s = \"Teee\\\"est\"; }");
    }


    @Test
    public void testStringLitEscaped2() throws Exception {
        assertValid("{ String s = \"line1\\nline2\\t\\n\\r\\\"\\\\\"; }");
    }
    
    @Test
    public void testStringCompareOps() throws Exception {
        assertValid("{ Bool b = \"Test\" == \"Test\"; }");
        assertValid("{ Bool b = \"Test\" != \"Test\"; }");
    }
    
    @Test(expected= JavaCodeGenerationException.class)
    public void testModuleNamedMain() throws Exception {
        // see bug #272
        assertValid("module Main; interface I { Unit m(); } class C implements I { Unit m() { }  } { }");
    }

}
