/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.delta;

import static org.junit.Assert.assertTrue;

import java.util.Arrays;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

import abs.common.WrongProgramArgumentException;
import abs.frontend.ast.*;

@RunWith(Parameterized.class)
public class DeltaAttributesIntegerTest extends DeltaTest {
    protected String product;
    protected String expected;
    public DeltaAttributesIntegerTest(String p, String x) {
        this.product = p;
        this.expected = x;
    }
    
    @Parameters
    public static java.util.Collection<?> data() {
        final Object[][] data = new String[][] {
                {"P1", "IntLiteral(0)"},
                {"P2", "IntLiteral(99)"},
        };
        return Arrays.asList(data);
    }
    
    @Test
    public void passIntegerFeatureAttribute() throws DeltaModellingException, WrongProgramArgumentException {
        Model model = assertParseOk(
                "module M;"
                + "delta D(Int attr);"
                +"uses M;"
                + "    adds class C { Int myField = attr; }"
                + "productline PL;"
                + "    features F;"
                + "    delta D(F.a) when F;"
                + "product P1(F{a=0});"
                + "product P2(F{a=99});"
                // TODO: test what happens if attribute is not passed
                // + "product P3(F);"
        );
        
        model.evaluateAllProductDeclarations();
        model.flattenForProduct(product);
        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertTrue(cls.getField(0).getName().equals("myField"));
//        System.out.println("******** expected: " + expected + " *** found: " + cls.getField(0).getInitExp().value.toString());

        assertTrue(cls.getField(0).getInitExp().value.toString().equals(expected));
    }

    public void passIntegerConstant() throws DeltaModellingException, WrongProgramArgumentException {
        Model model = assertParseOk(
                "module M;"
                + "delta D(Int attr);"
                +" uses M;"
                + "    adds class C { Int myField = attr; }"
                + "productline PL;"
                + "    features A,B;"
                + "    delta D(0) when A;"
                + "    delta D(99) when B;"
                + "product P1(A);"
                + "product P2(B);"
        );
       
        model.flattenForProduct(product);
        model.evaluateAllProductDeclarations();
        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertTrue(cls.getField(0).getName().equals("myField"));

        assertTrue(cls.getField(0).getInitExp().value.toString().equals(expected));
    }

}

