/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.delta;

import static org.junit.Assert.*;

import java.util.Arrays;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

import abs.common.WrongProgramArgumentException;
import abs.frontend.ast.*;
import choco.cp.solver.constraints.global.IncreasingNValue.Mode;

@RunWith(Parameterized.class)
public class DeltaAttributesBooleanTest extends DeltaTest {
    protected String product;
    protected String expected;
    public DeltaAttributesBooleanTest(String p, String x) {
        this.product = p;
        this.expected = x;
    }
    
    @Parameters
    public static java.util.Collection<?> data() {
        final Object[][] data = new String[][] {
                {"P1", "True()"},
                {"P2", "False()"}
        };
        return Arrays.asList(data);
    }
    
    @Test
    public void passFeatureAsBoolean() throws DeltaModellingException, WrongProgramArgumentException {
        Model model = assertParseOk(
                "module M;"
                + "delta D(Bool f);"
                + "uses M;"
                + "    adds class C { Bool myField = f; }" 
                + "productline PL;"
                + "    features F, G;"
                + "    delta D(F) when G;"
                + "product P1(F, G);"
                + "product P2(G);"
        );
        model.evaluateAllProductDeclarations();
        model.flattenForProduct(product);
        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertTrue(cls.getField(0).getName().equals("myField"));
        assertTrue(cls.getField(0).getInitExp().value.toString().equals(expected));
    }

    @Test
    public void passFeatureAsBoolean2() throws DeltaModellingException, WrongProgramArgumentException {
        Model model = assertParseOk(
                "module M;"
                        + "delta D(Bool a, Bool b, Bool c);"
                        + "uses M;"
                        + "    adds class C { Bool featureA = a; Bool featureB = b; Bool featureC = c; }"
                        + "productline PL;"
                        + "    features A,B,C,F;"
                        + "    delta D(A,B,C) when F;"
                        + "product P1(F,A);"
                        + "product P2(F,B);"
                        + "root F { group [0..*] { A, B, C } }"
                );
        
        model.evaluateAllProductDeclarations();
        model.flattenForProduct(product);
        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertTrue(cls.getField(0).getName().equals("featureA"));
//        assertTrue(cls.getField(1).getName().equals("featureB"));
//        assertTrue(cls.getField(2).getName().equals("featureC"));
        assertTrue(cls.getField(0).getInitExp().value.toString().equals(expected));
    }

    @Test
    public void passBooleanFeatureAttribute() throws DeltaModellingException, WrongProgramArgumentException {
        Model model = assertParseOk(
                "module M;"
                + "delta D(Bool attr);"
                + "uses M;"
                + "adds class C { Bool myField = attr; }"
                + "productline PL;"
                + "    features F;"
                + "    delta D(F.a) when F;"
                + "product P1(F{a=True});"
                + "product P2(F{a=False});"
        );
        model.evaluateAllProductDeclarations();
        model.flattenForProduct(product);
        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertTrue(cls.getField(0).getName().equals("myField"));
        assertTrue(cls.getField(0).getInitExp().value.toString().equals(expected));
    }

    @Test
    public void passBooleanConstant() throws DeltaModellingException, WrongProgramArgumentException {
        Model model = assertParseOk(
                "module M;"
                + "delta D(Bool attr);"
                + "uses M;"
                + "    adds class C { Bool myField = attr; }"
                + "productline PL;"
                + "    features A,B;"
                + "    delta D(True) when A;"
                + "    delta D(False) when B;"
                + "product P1(A);"
                + "product P2(B);"
        );
        model.evaluateAllProductDeclarations();
        model.flattenForProduct(product);
        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertEquals("myField", cls.getField(0).getName());
        assertEquals("Product " + product, expected, cls.getField(0).getInitExp().value.toString());
    }

}
