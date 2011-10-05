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
import abs.frontend.delta.exceptions.ASTNodeNotFoundException;

@RunWith(Parameterized.class)
public class DeltaAttributesBooleanTest extends DeltaFlattenerTest {
    protected String product;
    protected String expected;
    public DeltaAttributesBooleanTest(String p, String x) {
        this.product = p;
        this.expected = x;
    }
    
    @Parameters
    public static java.util.Collection<?> data() {
        final Object[][] data = new String[][] {
                {"M.P1", "True()"},
                {"M.P2", "False()"}
        };
        return Arrays.asList(data);
    }
    
    @Test
    public void passFeatureAsBoolean() throws ASTNodeNotFoundException, WrongProgramArgumentException {
        Model model = assertParseOk(
                "module M;"
                + "delta D(Bool f) { adds class C { Bool myField = f; } }" 
                + "productline PL { features F, G; delta D(F) when G; }"
                + "product P1(F, G);"
                + "product P2(G);"
        );
        
        model.flattenForProduct(product);
        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertTrue(cls.getField(0).getName().equals("myField"));
        assertTrue(cls.getField(0).getInitExp().value.toString().equals(expected));
    }

    @Test
    public void passBooleanFeatureAttribute() throws ASTNodeNotFoundException, WrongProgramArgumentException {
        Model model = assertParseOk(
                "module M;"
                + "delta D(Bool attr) { adds class C { Bool myField = attr; } }"
                + "productline PL { features F; delta D(F.a) when F; }"
                + "product P1(F{a=True});"
                + "product P2(F{a=False});"
        );
        
        model.flattenForProduct(product);
        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertTrue(cls.getField(0).getName().equals("myField"));
        assertTrue(cls.getField(0).getInitExp().value.toString().equals(expected));
    }
}
