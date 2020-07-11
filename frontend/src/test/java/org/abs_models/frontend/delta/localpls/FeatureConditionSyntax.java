package org.abs_models.frontend.delta.localpls;

import org.abs_models.common.WrongProgramArgumentException;
import org.abs_models.frontend.analyser.ErrorMessage;
import org.abs_models.frontend.ast.Model;
import org.abs_models.frontend.delta.DeltaTest;
import org.junit.Test;

import static org.junit.Assert.*;

public class FeatureConditionSyntax extends DeltaTest {
    @Test
    public void invalidProduct() throws WrongProgramArgumentException {
        Model model = assertParse(
            "module M;"
                + "base class C {}"
                + "features A, B with A && !B;"
                + "delta D1;"
                + "modifies class C {"
                + " adds Unit method1() { }"
                + "}"
                + "delta D2;"
                + "modifies class C {"
                + " adds Int method2() { }"
                + "}"
                + "delta D1 when A && !B;"
                + "delta D2 when B;"
                + "module Main;"
                + "product P1 = {A,B};" +
                "{}", Config.TYPE_CHECK, Config.EXPECT_TYPE_ERROR);
        assertEquals(ErrorMessage.INVALID_PRODUCT_LOCAL,model.getTypeErrors().getFirstError().msg);
        assertFalse(model.findProduct("P1").satisfiesConstraints());
    }
    @Test
    public void biImplFeatureCondition() throws WrongProgramArgumentException {
        Model model = assertParse(
            "module M;"
                + "base class C {}"
                + "features A, B, C, D with A <-> B;"
                + "delta D1;"
                + "modifies class C {"
                + " adds Unit method1() { }"
                + "}"
                + "delta D2;"
                + "modifies class C {"
                + " adds Int method2() { }"
                + "}"
                + "delta D1 when A;"
                + "delta D2 when B;"
                + "product P1 = {A, C};"
                + "product P2 = {B, D};"
                + "product P3 = {A, B};", Config.TYPE_CHECK, Config.EXPECT_TYPE_ERROR);


        assertEquals(ErrorMessage.INVALID_PRODUCT_LOCAL,model.getTypeErrors().getFirstError().msg);
        assertFalse(model.findProduct("P1").satisfiesConstraints());
        assertFalse(model.findProduct("P2").satisfiesConstraints());
        assertTrue(model.findProduct("P3").satisfiesConstraints());
    }

    @Test
    public void implFeatureCondition() throws WrongProgramArgumentException {
        Model model = assertParse(
            "module M;"
                + "base class C {}"
                + "features A, B, C with A -> B;"
                + "delta D1;"
                + "modifies class C {"
                + " adds Unit method1() { }"
                + "}"
                + "delta D2;"
                + "modifies class C {"
                + " adds Int method2() { }"
                + "}"
                + "delta D3;"
                + "adds class C1 {"
                + "Int method3(Int x) { }"
                + "}"
                + "delta D1 when A;"
                + "delta D2 when B;"
                + "delta D3 when C;"
                + "product P1 = {A, C};"
                + "product P2 = {B, D};"
                + "product P3 = {A, B};", Config.TYPE_CHECK, Config.EXPECT_TYPE_ERROR);


        assertEquals(ErrorMessage.INVALID_PRODUCT_LOCAL,model.getTypeErrors().getFirstError().msg);
        assertFalse(model.findProduct("P1").satisfiesConstraints());
        assertTrue(model.findProduct("P2").satisfiesConstraints());
        assertTrue(model.findProduct("P3").satisfiesConstraints());
    }

    @Test
    public void andOrFeatureCondition() throws WrongProgramArgumentException {
        Model model = assertParse(
            "module M;"
                + "base class C {}"
                + "features A, B, C, D with A && B || C;"
                + "delta D1;"
                + "modifies class C {"
                + " adds Unit method1() { }"
                + "}"
                + "delta D2;"
                + "modifies class C {"
                + " adds Int method2() { }"
                + "}"
                + "delta D3;"
                + "adds class C1 {"
                + "Int method3(Int x) { }"
                + "}"
                + "delta D1 when A;"
                + "delta D2 when B;"
                + "delta D3 when C;"
                + "product P1 = {A, C};"
                + "product P2 = {B, D};"
                + "product P3 = {A, B};"
                + "product P4 = {D};", Config.TYPE_CHECK, Config.EXPECT_TYPE_ERROR);


        assertEquals(ErrorMessage.INVALID_PRODUCT_LOCAL,model.getTypeErrors().getFirstError().msg);
        assertTrue(model.findProduct("P1").satisfiesConstraints());
        assertFalse(model.findProduct("P2").satisfiesConstraints());
        assertTrue(model.findProduct("P3").satisfiesConstraints());
        assertFalse(model.findProduct("P4").satisfiesConstraints());
    }

    @Test
    public void andOrFeatureConditionParenthesis() throws WrongProgramArgumentException {
        Model model = assertParse(
            "module M;"
                + "base class C {}"
                + "features A, B, C, D with A && (B || C);"
                + "delta D1;"
                + "modifies class C {"
                + " adds Unit method1() { }"
                + "}"
                + "delta D2;"
                + "modifies class C {"
                + " adds Int method2() { }"
                + "}"
                + "delta D3;"
                + "adds class C1 {"
                + "Int method3(Int x) { }"
                + "}"
                + "delta D1 when A;"
                + "delta D2 when B;"
                + "delta D3 when C;"
                + "product P1 = {A, C};"
                + "product P2 = {B, D};"
                + "product P3 = {A, B};"
                + "product P4 = {D};", Config.TYPE_CHECK, Config.EXPECT_TYPE_ERROR);


        assertEquals(ErrorMessage.INVALID_PRODUCT_LOCAL,model.getTypeErrors().getFirstError().msg);
        assertTrue(model.findProduct("P1").satisfiesConstraints());
        assertFalse(model.findProduct("P2").satisfiesConstraints());
        assertTrue(model.findProduct("P3").satisfiesConstraints());
        assertFalse(model.findProduct("P4").satisfiesConstraints());
    }

}
