/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.delta;

import java.util.ArrayList;
import java.util.Arrays;

import org.junit.Test;

import static org.hamcrest.CoreMatchers.instanceOf;
import static org.junit.Assert.*;
import abs.frontend.ast.*;


public class AddRemoveModifyClassesTest extends DeltaTest {
    @Test
    public void addClass() throws DeltaModellingException {
        Model model = assertParseOk(
                "module M;"
                + "delta D; uses M;"
                + "adds class C1(String s) {}"               
                + "adds class C2(String s) {}"
        );
        
        DeltaDecl delta = findDelta(model, "D");
        assertNotNull(delta);
        assertThat(delta, instanceOf(DeltaDecl.class));

        model.applyDelta(delta);
        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C1");
        assertNotNull(cls);
        assertThat(cls, instanceOf(ClassDecl.class));
        assertEquals("C1", cls.getName());
        cls = (ClassDecl) findDecl(model, "M", "C2");
        assertNotNull(cls);
        assertThat(cls, instanceOf(ClassDecl.class));
        assertEquals("C2", cls.getName());
    }

    @Test
    public void removeClass() throws DeltaModellingException {
        Model model = assertParseOk(
                "module M;"
                + "class C {}"
                + "delta D;"
                + "removes class M.C;"
        );

        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertNotNull(cls);
        DeltaDecl delta = findDelta(model, "D");
        assertNotNull(delta);

        model.applyDelta(delta);
        cls = (ClassDecl) findDecl(model, "M", "C");
        assertNull(cls);
    }
    
    @Test
    public void addField() throws DeltaModellingException {
        Model model = assertParseOk(
                "module M;"
                + "class C {}"
                + "delta D;"
                + "modifies class M.C {"
                + "    adds String myField = \"hello\";"
                + "}"
        );

        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertNotNull(cls);
        assertTrue(cls.getFields().getNumChild() == 0);
        DeltaDecl delta = findDelta(model, "D");
        assertNotNull(delta);

        model.applyDelta(delta);
        assertTrue(cls.getFields().getNumChild() == 1);
        assertTrue(cls.getField(0).getName().equals("myField"));
    }
    
    @Test
    public void removeField() throws DeltaModellingException {
        Model model = assertParseOk(
                "module M;"
                + "class C { String myField = \"hello\"; } " 
                + "delta D;"
                + "modifies class M.C { removes String myField; }"
        );

        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertNotNull(cls);
        assertTrue(cls.getFields().getNumChild() == 1);
        assertTrue(cls.getField(0).getName().equals("myField"));
        DeltaDecl delta = findDelta(model, "D");
        assertNotNull(delta);

        model.applyDelta(delta);
        assertEquals(0, cls.getFields().getNumChild());
    }
    
    @Test
    public void modifyField() throws DeltaModellingException {
        // remove and re-add field with different type
        Model model = assertParseOk(
                "module M; "
                + "class C { String f; Unit m() { f = \"Hello\"; } } "
                + "delta D1; "
                + "modifies class M.C { removes String f; }"
                + "delta D2; "
                + "modifies class M.C { adds Int f; modifies Unit m() { f = 99; } }"
        );

        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        DeltaDecl d1 = findDelta(model, "D1");
        DeltaDecl d2 = findDelta(model, "D2");

        model.applyDeltas(new ArrayList<DeltaDecl>(Arrays.asList(d1,d2)));
        assertTrue(cls.getFields().getNumChild() == 1);
        assertTrue(cls.getField(0).getName().equals("f"));
        assertTrue(cls.getField(0).getAccess().toString().equals("Int"));
    }
    
    @Test
    public void addMethod() throws DeltaModellingException {
        Model model = assertParseOk(
                "module M;"
                + "class C {}"
                + "delta D;"
                + "modifies class M.C { adds Unit myMethod() {} }"
        );

        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertNotNull(cls);
        assertTrue(cls.getMethods().getNumChild() == 0);
        DeltaDecl delta = findDelta(model, "D");
        assertNotNull(delta);

        model.applyDelta(delta);
        assertTrue(cls.getMethods().getNumChild() == 1);
        assertTrue(cls.getMethod(0).getMethodSig().getName().equals("myMethod"));
    }
    
    @Test
    public void removeMethod() throws DeltaModellingException {
        Model model = assertParseOk(
                "module M;"
                + "class C { Unit myMethod() {} }"
                + "delta D;"
                + "modifies class M.C { removes Unit myMethod(); }"
        );

        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertNotNull(cls);
        assertTrue(cls.getMethods().getNumChild() == 1);
        assertTrue(cls.getMethod(0).getMethodSig().getName().equals("myMethod"));
        DeltaDecl delta = findDelta(model, "D");
        assertNotNull(delta);

        model.applyDelta(delta);
        assertTrue(cls.getMethods().getNumChild() == 0);
    }
    
    @Test
    public void modifyMethod() throws DeltaModellingException {
        Model model = assertParseOk(
                "module M;"
                + "class C { Int myField = 0; Unit myMethod() {} } "
                + "delta D;"
                + "modifies class M.C {"
                + "    modifies Unit myMethod() { myField = 1; }"
                + "}"
        );

        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertNotNull(cls);
        assertTrue(cls.getMethods().getNumChild() == 1);
        assertTrue(cls.getMethod(0).getMethodSig().getName().equals("myMethod"));
        DeltaDecl delta = findDelta(model, "D");
        assertNotNull(delta);

        model.applyDelta(delta);
        assertTrue(cls.getMethods().getNumChild() == 1);
        assertTrue(cls.getMethod(0).getMethodSig().getName().equals("myMethod"));
        
        // make sure the MethodImpl defined in the delta is now in the class
        ModifyClassModifier cm = (ModifyClassModifier) delta.getModuleModifier(0);
        ModifyMethodModifier mm = (ModifyMethodModifier) cm.getModifier(0);
        // It's a bit of apples (FieldUse) vs. oranges (VarUse), but the strings look the same.
        assertEquals(cls.getMethod(0).toString(),mm.getMethodImpl().toString());
    }
}
