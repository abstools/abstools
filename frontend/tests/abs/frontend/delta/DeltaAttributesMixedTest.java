/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.delta;

import static org.junit.Assert.*;

import org.junit.Test;
import org.hamcrest.core.IsInstanceOf;

import abs.common.WrongProgramArgumentException;
import abs.frontend.analyser.ErrorMessage;
import abs.frontend.ast.*;
import abs.frontend.parser.SyntaxError;

public class DeltaAttributesMixedTest extends DeltaTest {

    @Test
    public void passFeaturesAsBooleans() throws DeltaModellingException, WrongProgramArgumentException {
        Model model = assertParseOk(
                "module M;"
                + "delta D(Bool a, Bool b, Bool c, Int c_a1);"
                + "uses M;"
                + "    adds class C { "
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

        model.evaluateAllProductDeclarations();
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
                + "uses M;"
                + "    adds class C { Bool first = a1; Bool second = a2; }"
                + "productline PL;"
                + "    features F;"
                + "    delta D(F.a, F.b) when F;"
                + "product P1( F{a=True, b=False} );"
        );
        
        model.evaluateAllProductDeclarations();
        model.flattenForProduct("P1");
        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertEquals("first", cls.getField(0).getName());
        assertEquals("True()", cls.getField(0).getInitExp().value.toString());
        assertEquals("second", cls.getField(1).getName());
        assertEquals("False()", cls.getField(1).getInitExp().value.toString());
    }

    @Test
    public void passBooleanFeatureAttributes1b() throws DeltaModellingException, WrongProgramArgumentException {
        Model model = assertParseOk(
                "module M;"
                + "delta D(Bool a1, Int a2);"
                + "uses M;"
                + "    adds class C { Bool first = a1; Int second = a2; }"
                + "productline PL;"
                + "    features F;"
                + "    delta D(F.a, F.b) when F;"
                + "product P1( F{a=True, b=3} );"
                , Config.WITH_STD_LIB, Config.TYPE_CHECK
        );

        model.evaluateAllProductDeclarations();
        model.flattenForProduct("P1");
        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertEquals("first", cls.getField(0).getName());
        assertEquals("True()", cls.getField(0).getInitExp().value.toString());
        assertEquals("second", cls.getField(1).getName());
        assertEquals("IntLiteral(3)", cls.getField(1).getInitExp().value.toString());
    }

    @Test
    public void passBooleanFeatureAttributes2() throws DeltaModellingException, WrongProgramArgumentException {
        Model model = assertParseOk(
                "module M;"
                + "delta D(Bool attr);"
                + "uses M;"
                + "    adds class C { Bool attr = attr; Unit m() {Bool x = attr;} }"
                + "productline PL;"
                + "    features F; delta D(F.a) when F;"
                + "product P1( F{a=True} );"
        );

        model.evaluateAllProductDeclarations();
        model.flattenForProduct("P1");
        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertEquals("attr", cls.getField(0).getName());
        assertEquals("True()", cls.getField(0).getInitExp().value.toString());

        //TODO test the value of x
    }

    @Test(expected=DeltaModellingException.class)
    public void passBooleanFeatureAttributes2b() throws Exception {
        Model model = assertParseOk(
                "module M;"
                + "delta D(Bool attr);"
                + "uses M;"
                + "    adds class C { Bool attr = attr; Unit m() {Bool x = attr;} }"
                + "productline PL;"
                + "    features F; delta D(F.b) when F;"
                + "product P1( F{a=True} );"
        );

        model.evaluateAllProductDeclarations();
        model.flattenForProduct("P1");
    }

    @Test
    public void passBooleanFeatureAttributes3() throws DeltaModellingException, WrongProgramArgumentException {
        Model m = assertParse(
                "module M;"
                + "delta D(Bool a1, Bool a2, Bool a3);"
                + "uses M;"
                + "    adds class C { Bool a1 = a1; Bool a2 = a2; Bool a3 = a3; }"
                + "productline PL;"
                + "    features A; delta D(A, B) when A;"
                + "product P1(A);",
                Config.TYPE_CHECK, Config.EXPECT_TYPE_ERROR);

        // There should be a type error if the Config defines only two delta params, but the delta itself expects 3
        assertEquals(ErrorMessage.WRONG_NUMBER_OF_ARGS, m.getTypeErrors().getFirstError().msg);
    }

    @Test
    public void passBooleanFeatureAttributes4() throws Exception {
        Model model = assertParse(
                "module M;"
                + "delta D(Bool attr);"
                + "uses M;"
                + "    adds class C { Bool attr = attr; Unit m() {Bool x = attr;} }"
                + "productline PL;"
                + "    features F; delta D(F.a) when F;"
                + "product P1( F{a=3} );"
                , Config.WITH_STD_LIB, Config.TYPE_CHECK, Config.EXPECT_TYPE_ERROR
        );
        assertEquals(ErrorMessage.CANNOT_ASSIGN,model.getTypeErrors().getFirstError().msg);
    }

    @Test
    public void passBooleanFeatureAttributes4b() throws Exception {
        Model model = assertParse(
                "module M;"
                + "delta D(Bool attr);"
                + "uses M;"
                + "    adds class C { Bool attr = attr; Unit m() {Bool x = attr;} }"
                + "productline PL;"
                + "    features F; delta D(3) when F;"
                + "product P1( F );"
                , Config.WITH_STD_LIB, Config.TYPE_CHECK, Config.EXPECT_TYPE_ERROR
        );
        assertEquals(ErrorMessage.CANNOT_ASSIGN,model.getTypeErrors().getFirstError().msg);
    }

    @Test
    public void passBooleanFeatureAttributes4Flat() throws Exception {
        Model model = assertParseOk(
                "module M;"
                + "delta D(Bool attr);"
                + "uses M;"
                + "    adds class C { Bool attr = attr; Unit m() {Bool x = attr;} }"
                + "productline PL;"
                + "    features F; delta D(F.a) when F;"
                + "product P1( F{a=3} );"
                , Config.WITH_STD_LIB
        );
        
        model.evaluateAllProductDeclarations();
        model.flattenForProduct("P1");
        assertTrue(model.hasTypeErrors());
        assertEquals(ErrorMessage.CANNOT_ASSIGN,model.getTypeErrors().getFirstError().msg);
    }

    @Test
    public void deltaParserNPE1() throws Exception {
        assertParseError(
                "module M;"
                + "delta D(Bool attr);"
                + "uses M;"
                + "    adds class C { Bool attr = attr; Unit m() {Bool x = attr;} }"
                + "productline PL;"
                + "    features F; delta D(F.a) when F;"
                + "product P1( F{a=} );"
        );
    }

    @Test
    public void deltaParserIlltyped() throws Exception {
        Model model = assertParseOk(
                "module M;"
                + "delta D(Bool attr);"
                + "uses M;"
                + "    adds class C { Bool attr = attr; Unit m() {Bool x = attr;} }"
                + "productline PL;"
                + "    features F; delta D(F.a) when F;"
                + "product P1( F{a=Blue} );"
        );
        model.evaluateAllProductDeclarations();
        model.flattenForProduct("P1");
        assertTrue(model.hasTypeErrors());
    }
}
