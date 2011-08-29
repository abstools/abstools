/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.delta;

import org.junit.Test;
import static org.junit.Assert.*;

import abs.frontend.ast.*;
import abs.frontend.delta.exceptions.*;

public class AddRemoveInterfacesTest extends DeltaFlattenerTest {

    @Test
    public void addIface1() throws ASTNodeNotFoundException {
        Model model = assertParseOk(
                "module M;"
                + "interface I {}"
                + "class C implements I { Unit m() {} }"
                + "delta D { "
                + "adds interface J {}"
                + "modifies class C implements J { modifies Unit m() {} }"
                + "}"
        );

        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertNotNull(cls);
        InterfaceDecl iface = (InterfaceDecl) findDecl(model, "M", "I");
        assertNotNull(iface);
        DeltaDecl delta = (DeltaDecl) findDecl(model, "M", "D");
        assertNotNull(delta);

        assertTrue(cls.getImplementedInterfaceUses().getNumChild() == 1);
        assertTrue(cls.getImplementedInterfaceUse(0).getName().equals("I"));
        
        model.applyDelta(delta);
        
        iface = (InterfaceDecl) findDecl(model, "M", "J");
        assertNotNull(iface);

        // make sure the class now also implements interface J
        assertTrue(cls.getImplementedInterfaceUses().getNumChild() == 2);
        assertTrue(cls.getImplementedInterfaceUse(0).getName().equals("I"));
        assertTrue(cls.getImplementedInterfaceUse(1).getName().equals("J"));
    }

    @Test
    public void addIface2() throws ASTNodeNotFoundException {
        Model model = assertParseOk(
                "module M;"
                + "interface I {}"
                + "class C implements I { Unit m() {} }"
                + "delta D { "
                + "adds interface J extends I {}"
                + "modifies class C implements J { modifies Unit m() {} }"
                + "}"
        );

        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertNotNull(cls);
        InterfaceDecl iface = (InterfaceDecl) findDecl(model, "M", "I");
        assertNotNull(iface);
        DeltaDecl delta = (DeltaDecl) findDecl(model, "M", "D");
        assertNotNull(delta);

        assertTrue(cls.getMethods().getNumChild() == 1);
        assertTrue(cls.getImplementedInterfaceUses().getNumChild() == 1);
        assertTrue(cls.getImplementedInterfaceUse(0).getName().equals("I"));
        
        model.applyDelta(delta);
        
        assertTrue(cls.getImplementedInterfaceUses().getNumChild() == 2);
        assertTrue(cls.getImplementedInterfaceUse(0).getName().equals("I"));
        assertTrue(cls.getImplementedInterfaceUse(1).getName().equals("J"));
    }

    @Test
    public void addIface3() throws ASTNodeNotFoundException {
        Model model = assertParseOk(
                "module M;"
                + "interface I {}"
                + "class C implements I { Unit m() {} }"
                + "delta D { "
                + "adds interface J {}"
                + "modifies class C implements I,J { modifies Unit m() {} } "
                + "}"
        );
        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        DeltaDecl delta = (DeltaDecl) findDecl(model, "M", "D");
        model.applyDelta(delta);
        
        // make sure the class implements I and J
        // FIXME!!!
        assertTrue(cls.getImplementedInterfaceUseList().getNumChild() == 3);
        assertTrue(cls.getImplementedInterfaceUse(0).getName().equals("I"));
        assertTrue(cls.getImplementedInterfaceUse(1).getName().equals("I"));
        assertTrue(cls.getImplementedInterfaceUse(2).getName().equals("J"));
    }

    @Test
    public void keepIface() throws ASTNodeNotFoundException {
        Model model = assertParseOk(
                "module M;"
                + "interface I {}"
                + "class C implements I { Unit m() {} }"
                + "delta D { "
                + "modifies class C { modifies Unit m() {} } "
                + "}"
        );
        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        DeltaDecl delta = (DeltaDecl) findDecl(model, "M", "D");
        model.applyDelta(delta);
        
        // make sure the class still implements interface I
        assertTrue(cls.getImplementedInterfaceUseList().getNumChild() == 1);
        assertTrue(cls.getImplementedInterfaceUse(0).getName().equals("I"));
    }


}
