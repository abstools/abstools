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
    public void notBaseeDecls(){
        Model model = assertParse(
            "module M;"
                + "interface I {}"
                + "class C implements I {}"
                + "features A, B with A && !B;"
                + "delta D1;"
                + "modifies class C {"
                + " adds Unit method1() { }"
                + "}");

        assertTrue(model.hasLocalProductLines());
        SemanticConditionList plErrors = model.getProductLineErrors();
        assertEquals(1, plErrors.getErrorCount());
        assertEquals(ErrorMessage.MISSING_BASE_IN_CLASS, plErrors.getFirstError().msg);

    }

    @Test
    public void notBaseInterfaceDecl(){
        Model model = assertParse(
            "module M;"
                + "interface I { }"
                + "features A, B with A && !B;"
                + "delta D1;"
                + "modifies interface I {"
                + " adds Unit method1();"
                + "}");

        assertTrue(model.hasLocalProductLines());
        SemanticConditionList plErrors = model.getProductLineErrors();
        assertEquals(1, plErrors.getErrorCount());
        assertEquals(ErrorMessage.MISSING_BASE_IN_INTERFACE, plErrors.getFirstError().msg);
    }

    @Test
    public void notRelativeDecls1() {
        Model model = assertParse(
            "module M;"
                + "base interface I {}"
                + "class C implements I {}"
                + "features A, B with A && !B;"
                + "delta D1;"
                + "modifies interface I {"
                + " adds Unit method1();"
                + "}");

        assertTrue(model.hasLocalProductLines());
        SemanticConditionList plErrors = model.getProductLineErrors();
        assertEquals(1, plErrors.getErrorCount());
        assertEquals(ErrorMessage.MISSING_RELATIVE_IN_CLASS, plErrors.getFirstError().msg);
    }

    @Test
    public void notRelativeDecls2() throws WrongProgramArgumentException {
        Model model = assertParse(
            "module M;"
                + "interface I {}"
                + "base class C (){ }"
                + "features A, B with A && !B;"
                + "delta D1;"
                + "adds class C2 () implements I {}"
                + "modifies class C {"
                + " adds Unit method1() {"
                + "  I field = new C2();"
                + " }"
                + "}"
                + "delta D1 when A;"
                + "product P2 = {A};");

        assertTrue(model.hasLocalProductLines());
        SemanticConditionList plErrors = model.getProductLineErrors();
        assertEquals(1, plErrors.getErrorCount());
        assertEquals(ErrorMessage.MISSING_RELATIVE_IN_CLASS, plErrors.getFirstError().msg);
    }


    @Test
    public void notRelativeDecls3() {
        Model model = assertParse(
            "module M;"
                + "interface I {}"
                + "base class C (){"
                + " Unit method1() {}"
                + "}"
                + "features A, B with A && !B;"
                + "delta D1;"
                + "adds class C2 () implements I {}"
                + "modifies class C {"
                + " modifies Unit method1() {"
                + "  I field = new C2();"
                + " }"
                + "}");

        assertTrue(model.hasLocalProductLines());
        model.collapseTraitModifiers();
        SemanticConditionList plErrors = model.getProductLineErrors();
        assertEquals(1, plErrors.getErrorCount());
        assertEquals(ErrorMessage.MISSING_RELATIVE_IN_CLASS, plErrors.getFirstError().msg);
    }


    @Test
    public void notRelativeDecls4() {
        Model model = assertParse(
            "module M;"
                + "base class C () {"
                + " Unit method1() {}"
                + "}"
                + "features A, B with A && !B;"
                + "delta D1;"
                + "adds interface I { }"
                + "modifies class C { "
                + "  adds I f;"
                + " }");

        assertTrue(model.hasLocalProductLines());
        SemanticConditionList plErrors = model.getProductLineErrors();
        assertEquals(1, plErrors.getErrorCount());
        assertEquals(ErrorMessage.MISSING_RELATIVE_IN_CLASS, plErrors.getFirstError().msg);
    }

    @Test
    public void notRelativeDecls5() {
        Model model = assertParse(
            "module M;"
                + "base interface I {}"
                + "features A, B with A && !B;"
                + "delta D1;"
                + "adds interface I2 {}"
                + "modifies interface I { "
                + "  adds I2 method();"
                + " }");

        assertTrue(model.hasLocalProductLines());
        SemanticConditionList plErrors = model.getProductLineErrors();
        assertEquals(1, plErrors.getErrorCount());
        assertEquals(ErrorMessage.MISSING_RELATIVE_IN_INTERFACE, plErrors.getFirstError().msg);
    }

    @Test
    public void notRelativeDecls6() {
        Model model = assertParse(
            "module M;"
                + "base class C {}"
                + "features A, B with A && !B;"
                + "delta D1;"
                + "adds interface I2 {}"
                + "modifies class C { "
                + "  adds I2 method() {}"
                + " }");

        assertTrue(model.hasLocalProductLines());
        SemanticConditionList plErrors = model.getProductLineErrors();
        assertEquals(1, plErrors.getErrorCount());
        assertEquals(ErrorMessage.MISSING_RELATIVE_IN_CLASS, plErrors.getFirstError().msg);
    }
    @Test
    public void notRelativeDecls7() {
        Model model = assertParse(
            "module M;"
                + "interface I {"
                + "   C m1();"
                + "}"
                + "base class C () {}"
                + "features A, B with A && !B;"
                + "delta D1;"
                + "modifies class C { "
                + "  adds Int c;"
                + " }");

        assertTrue(model.hasLocalProductLines());
        SemanticConditionList plErrors = model.getProductLineErrors();
        assertEquals(1, plErrors.getErrorCount());
        assertEquals(ErrorMessage.MISSING_RELATIVE_IN_INTERFACE, plErrors.getFirstError().msg);
    }

    @Test
    public void notRelativeDecls8() {
        Model model = assertParse(
            "module M;"
                + "interface I {"
                + "   C m1(C c);"
                + "}"
                + "base class C () {}"
                + "features A, B with A && !B;"
                + "delta D1;"
                + "modifies class C { "
                + "  adds Int c;"
                + " }");

        assertTrue(model.hasLocalProductLines());
        SemanticConditionList plErrors = model.getProductLineErrors();
        assertEquals(1, plErrors.getErrorCount());
        assertEquals(ErrorMessage.MISSING_RELATIVE_IN_INTERFACE, plErrors.getFirstError().msg);
    }
}
