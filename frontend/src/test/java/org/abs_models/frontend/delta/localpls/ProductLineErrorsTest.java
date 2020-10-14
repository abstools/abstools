package org.abs_models.frontend.delta.localpls;

import org.abs_models.ABSTest;
import org.abs_models.common.WrongProgramArgumentException;
import org.abs_models.frontend.analyser.ErrorMessage;
import org.abs_models.frontend.analyser.SemanticConditionList;
import org.abs_models.frontend.ast.Model;
import org.abs_models.frontend.delta.DeltaTest;
import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

public class ProductLineErrorsTest extends DeltaTest {
    @Test
    public void notUniqueDecls1(){
        Model model = assertParse(
            "module M;"
                + "unique interface I {}"
                + "unique class C implements I {}"
                + "features A, B with A && !B;"
                + "delta D1;"
                + "modifies class C {"
                + " adds Unit method1() { }"
                + "}");

        assertTrue(model.hasLocalProductLines());
        SemanticConditionList plErrors = model.getProductLineErrors();
        assertEquals(2, plErrors.getErrorCount());
        assertEquals(ErrorMessage.INVALID_UNIQUE_IN_CLASS, plErrors.getFirstError().msg);
    }

    public void notUniqueDecls2(){
        Model model = assertParse(
            "module M;"
                + "interface I {}"
                + "unique class C implements I {}"
                + "features A, B with A && !B;"
                + "delta D1;"
                + "modifies interface I {"
                + " adds Unit method1();"
                + "}");

        assertTrue(model.hasLocalProductLines());
        SemanticConditionList plErrors = model.getProductLineErrors();
        assertEquals(1, plErrors.getErrorCount());
        assertEquals(ErrorMessage.INVALID_UNIQUE_IN_INTERFACE, plErrors.getFirstError().msg);

    }


    public void configurationMissplaced(){
        Model model = assertParse(
            "module M;"
                + "configuration A = {F};"
                + "interface I {}"
                + "");

        assertTrue(model.hasLocalProductLines());
        SemanticConditionList plErrors = model.getProductLineErrors();
        assertEquals(1, plErrors.getErrorCount());
        assertEquals(ErrorMessage.PREPRODUCT_NOT_IN_VARIABLE, plErrors.getFirstError().msg);
    }

    public void configurationMissreference(){
        Model model = assertParse(
            "module M;"
                + "features F with F;"
                + "module N;"
                + "configuration A = {F}"
                + "features G with G;"
                + "");

        assertTrue(model.hasLocalProductLines());
        SemanticConditionList plErrors = model.getProductLineErrors();
        assertEquals(1, plErrors.getErrorCount());
        assertEquals(ErrorMessage.PREPRODUCT_NOT_LOCAL, plErrors.getFirstError().msg);
    }

}
