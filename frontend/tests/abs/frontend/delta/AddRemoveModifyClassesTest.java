/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.delta;

import org.junit.Test;
import static org.junit.Assert.*;

import abs.frontend.delta.exceptions.*;
import abs.frontend.ast.*;


public class AddRemoveModifyClassesTest extends DeltaFlattenerTest {
    @Test
    public void addClass() throws ASTNodeNotFoundException {
        Model model = assertParseOk("module M; delta D { adds class C {} }");
        
        DeltaDecl delta = (DeltaDecl) findDecl(model, "M", "D");
        assertNotNull(delta);
        
        model.applyDelta(delta);
        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertNotNull(cls);
    }

    @Test
    public void removeClass() throws ASTNodeNotFoundException {
        Model model = assertParseOk("module M; class C {} delta D { removes class C; }");
        
        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertNotNull(cls);
        DeltaDecl delta = (DeltaDecl) findDecl(model, "M", "D");
        assertNotNull(delta);

        model.applyDelta(delta);
        cls = (ClassDecl) findDecl(model, "M", "C");
        assertNull(cls);
    }
    
    @Test
    public void addField() throws ASTNodeNotFoundException {
        Model model = assertParseOk("module M; class C {} delta D { modifies class C { adds String myField = \"hello\"; } }");

        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertNotNull(cls);
        assertTrue(cls.getFields().getNumChild() == 0);
        DeltaDecl delta = (DeltaDecl) findDecl(model, "M", "D");
        assertNotNull(delta);

        model.applyDelta(delta);
        assertTrue(cls.getFields().getNumChild() == 1);
        assertTrue(cls.getField(0).getName().equals("myField"));
    }
    
    @Test
    public void removeField() throws ASTNodeNotFoundException {
        Model model = assertParseOk("module M; class C { String myField = \"hello\"; } delta D { modifies class C { removes String myField; } }");

        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertNotNull(cls);
        assertTrue(cls.getFields().getNumChild() == 1);
        assertTrue(cls.getField(0).getName().equals("myField"));
        DeltaDecl delta = (DeltaDecl) findDecl(model, "M", "D");
        assertNotNull(delta);

        model.applyDelta(delta);
        // FIXME!!
//        assertTrue(cls.getFields().getNumChild() == 0);
    }
    
    @Test
    public void addMethod() throws ASTNodeNotFoundException {
        Model model = assertParseOk("module M; class C {} delta D { modifies class C { adds Unit myMethod() {} } }");

        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertNotNull(cls);
        assertTrue(cls.getMethods().getNumChild() == 0);
        DeltaDecl delta = (DeltaDecl) findDecl(model, "M", "D");
        assertNotNull(delta);

        model.applyDelta(delta);
        assertTrue(cls.getMethods().getNumChild() == 1);
        assertTrue(cls.getMethod(0).getMethodSig().getName().equals("myMethod"));
    }
    
    @Test
    public void removeMethod() throws ASTNodeNotFoundException {
        Model model = assertParseOk("module M; class C { Unit myMethod() {} } delta D { modifies class C { removes Unit myMethod(); } }");

        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertNotNull(cls);
        assertTrue(cls.getMethods().getNumChild() == 1);
        assertTrue(cls.getMethod(0).getMethodSig().getName().equals("myMethod"));
        DeltaDecl delta = (DeltaDecl) findDecl(model, "M", "D");
        assertNotNull(delta);

        model.applyDelta(delta);
        assertTrue(cls.getMethods().getNumChild() == 0);
    }
    
    @Test
    public void modifyMethod() throws ASTNodeNotFoundException {
        Model model = assertParseOk("module M; class C { Int myField = 0; Unit myMethod() {} } delta D { modifies class C { modifies Unit myMethod() { myField = 1; } } }");

        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertNotNull(cls);
        assertTrue(cls.getMethods().getNumChild() == 1);
        assertTrue(cls.getMethod(0).getMethodSig().getName().equals("myMethod"));
        DeltaDecl delta = (DeltaDecl) findDecl(model, "M", "D");
        assertNotNull(delta);

        model.applyDelta(delta);
        assertTrue(cls.getMethods().getNumChild() == 1);
        assertTrue(cls.getMethod(0).getMethodSig().getName().equals("myMethod"));
        
        // make sure the MethodImpl defined in the delta is now in the class
        ModifyClassModifier cm = (ModifyClassModifier) delta.getClassOrIfaceModifier(0);
        ModifyMethodModifier mm = (ModifyMethodModifier) cm.getModifier(0);
        assertTrue(cls.getMethod(0).toString().equals(mm.getMethodImpl().toString()));
    }
}
