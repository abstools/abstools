/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.delta;

import static org.junit.Assert.*;
import org.junit.Test;
import abs.common.WrongProgramArgumentException;
import abs.frontend.ast.*;
import abs.frontend.delta.exceptions.DeltaModellingException;

public class DeltaAttributesMixedTest extends DeltaTest {

    @Test
    public void passFeaturesAsBooleans() throws DeltaModellingException, WrongProgramArgumentException {
        Model model = assertParseOk(
                "module M;"
                + "delta D(Bool a, Bool b, Bool c, Int c_a1);"
                + "    adds class M.C { "
                + "        Bool fA = a; "
                + "        Bool fB = b; "
                + "        Bool fC = c; "
                + "        Int fC_a1 = c_a1; "
                + "    }"
                + "productline PL;"
                + "    features A,B,C;"
                + "    delta D(A, B, C, C.a1) when C;"
                + "product P1( C{a1=99} );"
        );
        
        model.flattenForProduct("P1");
        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertEquals("fA", cls.getField(0).getName());
        assertEquals("False()", cls.getField(0).getInitExp().value.toString());
        assertEquals("fB", cls.getField(1).getName());
        assertEquals("False()", cls.getField(1).getInitExp().value.toString());
        assertEquals("fC", cls.getField(2).getName());
        assertEquals("True()", cls.getField(2).getInitExp().value.toString());
        assertEquals("fC_a1", cls.getField(3).getName());
        assertEquals("IntLiteral(99)", cls.getField(3).getInitExp().value.toString());
    }

    @Test
    public void passBooleanFeatureAttributes1() throws DeltaModellingException, WrongProgramArgumentException {
        Model model = assertParseOk(
                "module M;"
                + "delta D(Bool a1, Bool a2);"
                + "    adds class M.C { Bool first = a1; Bool second = a2; }"
                + "productline PL;"
                + "    features F;"
                + "    delta D(F.a, F.b) when F;"
                + "product P1( F{a=True, b=False} );"
        );
        
        model.flattenForProduct("P1");
        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertEquals("first", cls.getField(0).getName());
        assertEquals("True()", cls.getField(0).getInitExp().value.toString());
        assertEquals("second", cls.getField(1).getName());
        assertEquals("False()", cls.getField(1).getInitExp().value.toString());
    }

   
    @Test
    public void passBooleanFeatureAttributes2() throws DeltaModellingException, WrongProgramArgumentException {
        Model model = assertParseOk(
                "module M;"
                + "delta D(Bool attr);"
                + "    adds class M.C { Bool attr = attr; Unit m() {Bool x = attr;} }"
                + "productline PL;"
                + "    features F; delta D(F.a) when F;"
                + "product P1( F{a=True} );"
        );
        
        model.flattenForProduct("P1");
        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertEquals("attr", cls.getField(0).getName());
        assertEquals("True()", cls.getField(0).getInitExp().value.toString());
        
        //TODO test the value of x
    }

    @Test
    public void passBooleanFeatureAttributes3() throws DeltaModellingException, WrongProgramArgumentException {
        assertParse(
                "module M;"
                + "delta D(Bool a1, Bool a2, Bool a3);"
                + "    adds class C { Bool a1 = a1; Bool a2 = a2; Bool a3 = a3; }"
                + "productline PL;"
                + "    features F; delta D(A, B) when A;"
                + "product P1(A);",
                Config.TYPE_CHECK, Config.EXPECT_TYPE_ERROR);
        
        // There should be a type error if the Config defines only two delta params, but the delta itself expects 3
    
    }

    
}
