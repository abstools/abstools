package org.abs_models.frontend.delta.localpls;

import org.abs_models.frontend.ast.DeltaDecl;
import org.abs_models.frontend.ast.InterfaceDecl;
import org.abs_models.frontend.ast.Model;
import org.abs_models.frontend.delta.DeltaModellingException;
import org.abs_models.frontend.delta.DeltaTest;
import org.junit.Test;

import static org.junit.Assert.*;

public class AddRemoveModifyInterfaceLPLTest extends DeltaTest {
    @Test
    public void addInterface() throws DeltaModellingException {
        Model model = assertParse("module M;"
            + "features F1;"
            + "delta D;"
            + "adds interface I1 {}"
            + "adds interface I2 {}"
        );
        assertTrue(model.hasLocalProductLines());
        DeltaDecl delta = LocalPLsTest.findDelta(model, "M", "D");
        assertNotNull(delta);

        delta.apply();
        InterfaceDecl interface1 = (InterfaceDecl) findDecl(model, "M", "I1");
        assertNotNull(interface1);

        InterfaceDecl interface2 = (InterfaceDecl) findDecl(model, "M", "I2");
        assertNotNull(interface2);
    }
    @Test
    public void removeInterface() throws DeltaModellingException {
        Model model = assertParse("module M;"
            + "interface I{}"
            + "features F1;"
            + "delta D;"
            + "removes interface I;"
        );
        assertTrue(model.hasLocalProductLines());
        DeltaDecl delta = LocalPLsTest.findDelta(model, "M", "D");
        assertNotNull(delta);

        InterfaceDecl interface1 = (InterfaceDecl) findDecl(model, "M", "I");
        assertNotNull(interface1);

        delta.apply();

        interface1 = (InterfaceDecl) findDecl(model, "M", "I2");
        assertNull(interface1);
    }

    @Test
    public void modifyInterface1() throws DeltaModellingException {
        Model model = assertParse("module M;"
            + "interface I{}"
            + "features F1;"
            + "delta D;"
            + "modifies interface I {"
            + "adds Unit method1(Bool b);"
            + "}"
        );
        assertTrue(model.hasLocalProductLines());
        DeltaDecl delta = LocalPLsTest.findDelta(model, "M", "D");
        assertNotNull(delta);

        InterfaceDecl interface1 = (InterfaceDecl) findDecl(model, "M", "I");
        assertNotNull(interface1);
        assertEquals(0, interface1.getNumBody());
        delta.apply();

        assertEquals(1, interface1.getNumBody());
        assertEquals("method1", interface1.getBody(0).getName());
        assertEquals(1, interface1.getBody(0).getNumParam());
        assertEquals("b", interface1.getBody(0).getParam(0).getName());
    }
    @Test
    public void modifyInterface2() throws DeltaModellingException {
        Model model = assertParse("module M;"
            + "interface I{"
            + "Int method1();"
            + "}"
            + "features F1;"
            + "delta D;"
            + "modifies interface I {"
            + "removes Int method1();"
            + "}"
        );
        assertTrue(model.hasLocalProductLines());
        DeltaDecl delta = LocalPLsTest.findDelta(model, "M", "D");
        assertNotNull(delta);

        InterfaceDecl interface1 = (InterfaceDecl) findDecl(model, "M", "I");
        assertNotNull(interface1);

        assertEquals(1, interface1.getNumBody());
        assertEquals("method1", interface1.getBody(0).getName());

        delta.apply();

        assertEquals(0, interface1.getNumBody());
    }
}
