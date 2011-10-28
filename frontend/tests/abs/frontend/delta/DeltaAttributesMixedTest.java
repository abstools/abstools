/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.delta;

import static org.junit.Assert.assertTrue;
import org.junit.Test;
import abs.common.WrongProgramArgumentException;
import abs.frontend.ast.*;
import abs.frontend.delta.exceptions.ASTNodeNotFoundException;

public class DeltaAttributesMixedTest extends DeltaFlattenerTest {

    @Test
    public void passFeaturesAsBooleans() throws ASTNodeNotFoundException, WrongProgramArgumentException {
        Model model = assertParseOk(
                "module M;"
                + "delta D(Bool a, Bool b, Bool c, Int c_a1) { adds class C { "
                + "Bool fA = a; "
                + "Bool fB = b; "
                + "Bool fC = c; "
                + "Int fC_a1 = c_a1; "
                + "} }" 
                + "productline PL { features A,B,C; delta D(A, B, C, C.a1) when C; }"
                + "product P1( C{a1=99} );"
        );
        
        model.flattenForProduct("M.P1");
        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertTrue(cls.getField(0).getName().equals("fA"));
        assertTrue(cls.getField(0).getInitExp().value.toString().equals("False()"));
        assertTrue(cls.getField(1).getName().equals("fB"));
        assertTrue(cls.getField(1).getInitExp().value.toString().equals("False()"));
        assertTrue(cls.getField(2).getName().equals("fC"));
        assertTrue(cls.getField(2).getInitExp().value.toString().equals("True()"));
        assertTrue(cls.getField(3).getName().equals("fC_a1"));
        assertTrue(cls.getField(3).getInitExp().value.toString().equals("IntLiteral(99)"));
    }

    @Test
    public void passBooleanFeatureAttributes1() throws ASTNodeNotFoundException, WrongProgramArgumentException {
        Model model = assertParseOk(
                "module M;"
                + "delta D(Bool a1, Bool a2) { adds class C { Bool first = a1; Bool second = a2; } }"
                + "productline PL { features F; delta D(F.a, F.b) when F; }"
                + "product P1(F{a=True, b=False});"
        );
        
        model.flattenForProduct("M.P1");
        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertTrue(cls.getField(0).getName().equals("first"));
        assertTrue(cls.getField(0).getInitExp().value.toString().equals("True()"));
        assertTrue(cls.getField(1).getName().equals("second"));
        assertTrue(cls.getField(1).getInitExp().value.toString().equals("False()"));
    }

   
    @Test
    public void passBooleanFeatureAttributes2() throws ASTNodeNotFoundException, WrongProgramArgumentException {
        Model model = assertParseOk(
                "module M;"
                + "delta D(Bool attr) { adds class C { Bool attr = attr; } }"
                + "productline PL { features F; delta D(F.a) when F; }"
                + "product P1(F{a=True});"
        );
        
        model.flattenForProduct("M.P1");
        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertTrue(cls.getField(0).getName().equals("attr"));
        assertTrue(cls.getField(0).getInitExp().value.toString().equals("True()"));
    }

    @Test
    public void passBooleanFeatureAttributes3() throws ASTNodeNotFoundException, WrongProgramArgumentException {
        assertParse(
                "module M;"
                + "delta D(Bool a1, Bool a2, Bool a3) { adds class C { Bool a1 = a1; Bool a2 = a2; Bool a3 = a3; } }"
                + "productline PL { features F; delta D(A, B) when A; }"
                + "product P1(A);",
                Config.TYPE_CHECK, Config.EXPECT_TYPE_ERROR);
        
        // There should be a type error if the Config defines only two delta params, but the delta itself expects 3
    
    }

    
}
