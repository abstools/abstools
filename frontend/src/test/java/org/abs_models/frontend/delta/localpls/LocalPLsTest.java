package org.abs_models.frontend.delta.localpls;

import org.abs_models.common.WrongProgramArgumentException;
import org.abs_models.frontend.analyser.ErrorMessage;
import org.abs_models.frontend.analyser.SemanticConditionList;
import org.abs_models.frontend.ast.*;
import org.abs_models.frontend.delta.DeltaModellingException;
import org.abs_models.frontend.delta.DeltaTest;
import org.junit.Test;

import static org.junit.Assert.*;

public class LocalPLsTest extends DeltaTest {
    public static DeltaDecl findDelta(Model m, String moduleName, String name) {
        ModuleDecl module = m.lookupModule(moduleName);
        return module.findDelta(name);
    }
    @Test
    public void localProductline() throws DeltaModellingException {
        Model model = assertParse(
            "module M;"
                + "class C {}"
                + "features A, B;"
                + "delta D1;"
                + "modifies class C {"
                + " adds Unit method1() { }"
                + "}"
                + "delta D1 when A;");


        DeltaDecl delta = findDelta(model, "M", "D1");
        assertNotNull(delta);
    }

    @Test
    public void invalidProductFromPL(){
        Model model = assertParse(
            "module M;"
                + "class C {}"
                + "features A, B with A && !B;"
                + "delta D1;"
                + "modifies class C {"
                + " adds Unit method1() { }"
                + "}"
                + "product P1 = {C};", Config.TYPE_CHECK, Config.EXPECT_TYPE_ERROR);

        assertEquals(ErrorMessage.ERROR_IN_PRODUCT,model.getTypeErrors().getFirstError().msg);
    }

    @Test
    public void invalidPL(){

        Model model = assertParse("module M;"
            + "features A;"
            + "delta D1; modifies class C { adds Unit foo() {} }"
            + "delta D2; modifies class M.C { adds Unit foo() {} }"
            + "delta D2 after D1 when A;", Config.TYPE_CHECK, Config.EXPECT_TYPE_ERROR);

        assertEquals(1, model.getTypeErrors().getErrorCount());
    }
    @Test
    public void tryDelta() throws WrongProgramArgumentException {
        Model model = assertParse(
            "module M;"
                + "export *;"
                + "base interface I{}"
                + "features A;"
                + "delta D1;"
                + "modifies interface I {"
                + " adds Unit me1(); }"
                + "delta D1 when A;"
                +  "module M1;"
                + "base interface I{}"
                + "features B;"
                + "delta D2;"
                + "modifies interface I {"
                + "adds Int retMethod();"
                +  "}"
                + "module TestMain;"
                + "import * from M;"
                + " { I with LocalProduct m; }"
                +"product LocalProduct = {A};"
        );
        assertTrue(model.hasLocalProductLines());
        DeltaDecl delta = findDelta(model, "M", "D1");
        assertTrue(delta != null);
        DeltaDecl delta2 = findDelta(model, "M1", "D2");
        assertNotNull(delta2);
        assertTrue(delta2.getModuleModifier(0) != null);

        model.flattenforLocalProducts();
        model.typeCheck();
        assertEquals(0, model.getTypeErrors().getErrorCount());
        InterfaceDecl interface1 = (InterfaceDecl) findDecl(model, "M_A", "I");
        assertTrue(interface1 != null);

        InterfaceDecl interface2 = (InterfaceDecl) findDecl(model, "M1", "I");
        assertTrue(interface2 != null);
    }
    @Test
    public void applicationOrderTest() throws WrongProgramArgumentException {
        Model model = assertParse(
            "module M;"
                + " base class C () {"
                + "   Unit method1(){"
                + "       Int x = 3;"
                + "   }"
                + " }"
                + "features A, B;"
                + "delta D1;"
                + "modifies class C {"
                + "   adds Int method2(){ return 0;}"
                + " modifies Unit method1() {"
                + " Bool c = True;"
                + "}"
                + "}"
                + "delta D2;"
                + "modifies class C {"
                + "    modifies Int method2(){ return 50;}"
                + " modifies Unit method1() { Bool y = False;}"
                + "}"
                + "delta D3;"
                + "modifies class C {"
                + "modifies Unit method1() {"
                + "Int y = 3; }}"
                + "delta D1 when A;"
                + " delta D2 after D1 when A;"
                + "delta D3 when A;"
                + "D3 < D1;"
                + "product Rest = {A};");
        model.flattenforLocalProducts();
    }
}
