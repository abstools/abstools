package org.abs_models.frontend.delta.localpls;

import org.abs_models.frontend.ast.*;
import org.abs_models.frontend.delta.DeltaModellingException;
import org.abs_models.frontend.delta.DeltaTest;
import org.junit.Test;

import java.util.ArrayList;
import java.util.Arrays;

import static org.junit.Assert.*;

public class AddRemoveModifyClassesLPLTest extends DeltaTest {
    @Test
    public void addClass() throws DeltaModellingException {
        Model model = assertParse("module M;"
            + "features F1;"
            + "delta D;"
            + "adds class C1(String s) {}"
            + "adds class C2(String s) {}"
        );
        assertTrue(model.hasLocalProductLines());
        DeltaDecl delta = LocalPLsTest.findDelta(model, "M", "D");
        assertNotNull(delta);

        delta.apply();
        ClassDecl cls1 = (ClassDecl) findDecl(model, "M", "C1");
        assertNotNull(cls1);

        ClassDecl cls2 = (ClassDecl) findDecl(model, "M", "C2");
        assertNotNull(cls2);
    }

    @Test
    public void removeClass() throws DeltaModellingException {
        Model model = assertParse("module M;"
            + "class C {}"
            + "features F;"
            + "delta D;"
            + "removes class C;");

        assertTrue(model.hasLocalProductLines());
        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertNotNull(cls);
        DeltaDecl delta = LocalPLsTest.findDelta(model, "M", "D");
        assertNotNull(delta);

        delta.apply();
        cls = (ClassDecl) findDecl(model, "M", "C");
        assertNull(cls);
    }

    @Test
    public void addField() throws DeltaModellingException {
        Model model = assertParse("module M;"
            + "class C {}"
            + "features F;"
            + "delta D;"
            + "modifies class C {"
            + "    adds String myField = \"hello\";"
            + "}");

        assertTrue(model.hasLocalProductLines());
        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertNotNull(cls);
        assertTrue(cls.getFields().getNumChild() == 0);
        DeltaDecl delta = LocalPLsTest.findDelta(model, "M", "D");
        assertNotNull(delta);

        delta.apply();
        assertTrue(cls.getFields().getNumChild() == 1);
        assertTrue(cls.getField(0).getName().equals("myField"));
    }

    @Test
    public void removeField() throws DeltaModellingException {
        Model model = assertParse("module M;"
            + "class C { String myField = \"hello\"; } "
            + "features F;"
            + "delta D;"
            + "modifies class C { removes String myField; }");

        assertTrue(model.hasLocalProductLines());
        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertNotNull(cls);
        assertTrue(cls.getFields().getNumChild() == 1);
        assertTrue(cls.getField(0).getName().equals("myField"));
        DeltaDecl delta = LocalPLsTest.findDelta(model, "M", "D");
        assertNotNull(delta);

        delta.apply();
        assertEquals(0, cls.getFields().getNumChild());
    }

    @Test
    public void modifyField() throws DeltaModellingException {
        // remove and re-add field with different type
        Model model = assertParse("module M; "
            + "class C { String f; Unit m() { f = \"Hello\"; } } "
            + "features F;"
            + "delta D1; "
            + "modifies class C { removes String f; }"
            + "delta D2; "
            + "modifies class C { adds Int f; modifies Unit m() { f = 99; } }");

        assertTrue(model.hasLocalProductLines());
        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        DeltaDecl d1= LocalPLsTest.findDelta(model, "M", "D1");
        DeltaDecl d2 = LocalPLsTest.findDelta(model, "M", "D2");

        model.applyDeltas(new ArrayList<>(Arrays.asList(d1, d2)));
        assertTrue(cls.getFields().getNumChild() == 1);
        assertTrue(cls.getField(0).getName().equals("f"));
        assertTrue(cls.getField(0).getAccess().toString().equals("Int"));
    }

    @Test
    public void addMethod() throws DeltaModellingException {
        Model model = assertParse("module M;"
            + "class C {}"
            + "features F;"
            + "delta D;"
            + "modifies class C { adds Unit myMethod() {} }");

        assertTrue(model.hasLocalProductLines());
        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertNotNull(cls);
        assertTrue(cls.getMethods().getNumChild() == 0);
        DeltaDecl delta = LocalPLsTest.findDelta(model, "M", "D");
        assertNotNull(delta);

        delta.apply();
        assertTrue(cls.getMethods().getNumChild() == 1);
        assertTrue(cls.getMethod(0).getMethodSig().getName().equals("myMethod"));
    }

    @Test
    public void removeMethod() throws DeltaModellingException {
        Model model = assertParse("module M;"
            + "class C { Unit myMethod() {} }"
            + "features F;"
            + "delta D;"
            + "modifies class C { removes Unit myMethod(); }");

        assertTrue(model.hasLocalProductLines());
        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertNotNull(cls);
        assertTrue(cls.getMethods().getNumChild() == 1);
        assertTrue(cls.getMethod(0).getMethodSig().getName().equals("myMethod"));
        DeltaDecl delta = LocalPLsTest.findDelta(model, "M", "D");
        assertNotNull(delta);

        delta.apply();
        assertTrue(cls.getMethods().getNumChild() == 0);
    }

    @Test
    public void modifyMethod() throws DeltaModellingException {
        Model model = assertParse("module M;"
            + "class C { Int myField = 0; Unit myMethod() {} } "
            + "features F;"
            + "delta D;"
            + "modifies class C {"
            + "    modifies Unit myMethod() { myField = 1; }"
            + "}");

        assertTrue(model.hasLocalProductLines());
        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertNotNull(cls);
        assertTrue(cls.getMethods().getNumChild() == 1);
        assertTrue(cls.getMethod(0).getMethodSig().getName().equals("myMethod"));
        DeltaDecl delta = LocalPLsTest.findDelta(model, "M", "D");
        assertNotNull(delta);

        delta.apply();
        assertTrue(cls.getMethods().getNumChild() == 1);
        assertTrue(cls.getMethod(0).getMethodSig().getName().equals("myMethod"));

        // make sure the MethodImpl defined in the delta is now in the class
        ModifyClassModifier cm = (ModifyClassModifier) delta.getModuleModifier(0);
        DeltaTraitModifier mm = (DeltaTraitModifier) cm.getModifier(0);
        ModifyMethodModifier opr = (ModifyMethodModifier) mm.getMethodModifier();
        TraitSetExpr expr = (TraitSetExpr) opr.getTraitExpr();

        // It's a bit of apples (FieldUse) vs. oranges (VarUse), but the strings look the same.
        assertEquals(cls.getMethod(0).toString(), expr.getMethodImpl(0).toString());
    }
}
