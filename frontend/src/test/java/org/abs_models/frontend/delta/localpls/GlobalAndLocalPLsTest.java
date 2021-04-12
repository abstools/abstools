package org.abs_models.frontend.delta.localpls;

import org.abs_models.common.WrongProgramArgumentException;
import org.abs_models.frontend.ast.*;
import org.abs_models.frontend.delta.DeltaModellingException;
import org.abs_models.frontend.delta.DeltaTest;
import org.junit.Test;

import static junit.framework.TestCase.assertNotNull;
import static org.junit.Assert.*;

public class GlobalAndLocalPLsTest extends DeltaTest {
    String localPLString = "module M;"
        + "export *;"
        + "configuration LocalProduct = {A};"
        + "unique interface I1 {"
        + "}"
        + "class C1() implements I1 {}"
        + "features A, B;"
        + "delta LocalDeltaA;"
        + "adds interface I2{}"
        + "modifies class C1 {"
        + " adds String s = \"MC\";"
        + " adds Unit printS() {"
        + "   println(s);"
        + " }"
        + "}"
        + "delta LocalDeltaB;"
        + "modifies class C1 {"
        + " removes String s;"
        + " removes Unit printS();"
        + " adds Unit print1() {"
        + "   println(\"1\");"
        + " }"
        + "}"
        + "delta LocalDeltaA when A;"
        + "delta LocalDeltaB after LocalDeltaA when B;";

    String globalPLString = "delta GlobalDelta;"
        + "uses M;"
        + "modifies class C1 {"
        + " adds Int x = 5;"
        + " adds Unit printX() {"
        + "   println(toString(x));"
        + " } }"

        + "productline GlobalPL;"
        + "features C, D;"
        + "delta GlobalDelta when C;"
        + "product GlobalProduct = {C};";

    String mainModule = "module MainModule;"
        + "import * from M;"
        + "{"
        + "  I1 object1 = new C1() with LocalProduct;"
        + "}";


    @Test
    public void parseTest() throws WrongProgramArgumentException {
        Model model = assertParse( localPLString + mainModule + globalPLString

        );
        assertNotNull(model);
        InterfaceDecl interface1 = (InterfaceDecl) findDecl(model, "M", "I1");
        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C1");
        assertNotNull(cls);
        assertNotNull(interface1);
        assertEquals(0, cls.getMethods().getNumChild());
        assertEquals(0, cls.getFields().getNumChild());

        model.flattenForProduct("GlobalProduct");

        assertEquals(1, cls.getMethods().getNumChild());
        assertTrue(cls.getMethod(0).getMethodSig().getName().equals("printX"));
        assertEquals(1, cls.getFields().getNumChild());
        FieldDecl field = cls.getField(0);
        assertTrue(field.getName().equals("x"));
        assertTrue(field.getInitExp() instanceof IntLiteral);
        assertEquals("5", ((IntLiteral) field.getInitExp()).getContent());
        int numModulesBefore = model.getModuleDecls().size();

        assertEquals(0, model.getProductLineErrors().getErrorCount());
        model.flattenforLocalProducts();

        assertTrue(model.getModuleDecls().size() == (numModulesBefore + 1));
        ModuleDecl originalModule = model.lookupModule("M");
        ModuleDecl productModule = model.lookupModule("M_A");
        assertNotNull(originalModule);
        assertNotNull(productModule);

        assertTrue(originalModule.getNumDecl() == 1);
        interface1 = (InterfaceDecl) findDecl(model, "M", "I1");
        assertNotNull(interface1);
        interface1 = (InterfaceDecl) findDecl(model, "M_A", "I1");
        assertNull(interface1);
        interface1 = (InterfaceDecl) findDecl(model, "M_A", "I2");
        assertNotNull(interface1);

        cls = (ClassDecl) findDecl(model, "M", "C1");
        assertNull(cls);
        cls = (ClassDecl) findDecl(model, "M_A", "C1");
        assertNotNull(cls);

        assertEquals(2, cls.getMethods().getNumChild());
        assertTrue(cls.getMethod(0).getMethodSig().getName().equals("printX"));
        assertTrue(cls.getMethod(1).getMethodSig().getName().equals("printS"));
        assertEquals(2, cls.getFields().getNumChild());
        field = cls.getField(0);
        assertTrue(field.getName().equals("x"));
        field = cls.getField(1);
        assertTrue(field.getName().equals("s"));
    }

    @Test(expected = DeltaModellingException.class)
    public void invalidMixProduct() throws WrongProgramArgumentException {
        Model model = assertParse(localPLString + "module M2;" + globalPLString
                        + "product P1 = {A, C};");
        assertNotNull(model);
        assertTrue(model.getProductLineErrors().getErrorCount() == 0);
        model.flattenForProduct("P1");
    }

    @Test(expected = DeltaModellingException.class)
    public void invalidProductLocalFts() throws WrongProgramArgumentException {
        Model model = assertParse(localPLString + "module M2;" + globalPLString
            + "product P1 = {A};");
        assertNotNull(model);
        assertTrue(model.getProductLineErrors().getErrorCount() == 0);
        model.flattenForProduct("P1");
    }

    @Test
    public void globalPLwithLocalProducts() throws WrongProgramArgumentException {
        String globalPLString2 = "delta GlobalDelta2; "
            + "uses M;"
            + "adds unique class C2 () {"
            + " I1 with {A, B} obj;"
            + " }"

            + "productline GlobalPL;"
            + "features C, D;"
            + "delta GlobalDelta2 when C;";

        Model model = assertParse( localPLString + mainModule + globalPLString2
            + "product GlobalProduct = {C};"

        );
        model.flattenForProduct("GlobalProduct");
        model.flattenforLocalProducts();
        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C2");
        assertNotNull(cls);
    }


}
