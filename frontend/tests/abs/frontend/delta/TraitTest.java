/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.delta;

import static org.junit.Assert.*;

import org.junit.Test;

import abs.frontend.ast.ClassDecl;
import abs.frontend.ast.DeltaDecl;
import abs.frontend.ast.Model;
import abs.frontend.ast.SkipStmt;

public class TraitTest extends DeltaTest{

    @Test
    public void addMethod()  {
        Model model = assertParseOk(
                "module M;"
                + "trait T = { Unit myMethod(){ skip; } }"
                + "class C {adds T; }"
        );

        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertNotNull(cls);
        assertTrue(cls.getMethods().getNumChild() == 0);

        model.applyTraits();
        assertTrue(cls.getMethods().getNumChild() == 1);
        assertTrue(cls.getMethod(0).getMethodSig().getName().equals("myMethod"));
    }

    
    @Test
    public void addTwoMethods()  {
        Model model = assertParseOk(
                "module M;"
                + "trait T = { Unit myMethod(){ skip; } }"
                + "trait T2 = { Unit myMethod(){ skip; } }"
                + "class C {adds T; adds T2; }"
        );

        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertNotNull(cls);
        assertTrue(cls.getMethods().getNumChild() == 0);

        model.applyTraits();
        assertTrue(cls.getMethods().getNumChild() == 2);
        assertTrue(cls.getMethod(0).toString().equals(cls.getMethod(1).toString()));
        assertTrue(model.getErrors().containsErrors());
    }

    
    
    @Test(expected=DeltaModellingException.class)
    public void modifyNonExistingMethods()  {
        Model model = assertParseOk(
                "module M;"
                + "trait T = {} modifies { Unit myMethod(){ skip; } }"
                + "class C {adds T;  }"
        );

        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertNotNull(cls);
        assertTrue(cls.getMethods().getNumChild() == 0);

        model.applyTraits();
    }


    @Test
    public void modifyExistingMethod()  {
        Model model = assertParseOk(
                "module M; "
                + "trait T = {} modifies { Unit myMethod(){ skip; } } "
                + "class C {adds T; Unit myMethod(){ println(\"\"); } }"
        );

        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertNotNull(cls);
        assertTrue(cls.getMethods().getNumChild() == 1);

        model.applyTraits();
        assertTrue(cls.getMethods().getNumChild() == 1);
        assertTrue(cls.getMethod(0).getBlock().getStmt(0) instanceof SkipStmt);
    }
    
    @Test
    public void modifyTwiceExistingMethod()  {
        Model model = assertParseOk(
                "module M; "
                + "trait T = {} modifies { Unit myMethod(){ println(\"test\"); } } modifies { Unit myMethod(){ skip; } }"
                + "class C {adds T; Unit myMethod(){ println(\"\"); } }"
        );

        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertNotNull(cls);
        assertTrue(cls.getMethods().getNumChild() == 1);

        model.applyTraits();
        assertTrue(cls.getMethods().getNumChild() == 1);
        assertTrue(cls.getMethod(0).getBlock().getStmt(0) instanceof SkipStmt);
    }

    @Test
    public void modifyTwiceTwoTraitsExistingMethod()  {
        Model model = assertParseOk(
                "module M; "
                        + "trait T = {} modifies { Unit myMethod(){ println(\"test\"); } }"
                        + "trait T2 = {} modifies { Unit myMethod(){ skip; } }"
                + "class C {adds T;adds T2; Unit myMethod(){ println(\"\"); } }"
        );

        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertNotNull(cls);
        assertTrue(cls.getMethods().getNumChild() == 1);

        model.applyTraits();
        assertTrue(cls.getMethods().getNumChild() == 1);
        assertTrue(cls.getMethod(0).getBlock().getStmt(0) instanceof SkipStmt);
    }

    @Test
    public void addAndModifyExistingMethod()  {
        Model model = assertParseOk(
                "module M; "
                + " trait T = {Unit myMethod(){ println(\"\"); }} modifies { Unit myMethod(){ skip; } }"
                + " class C {adds T; }"
        );

        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertNotNull(cls);
        assertTrue(cls.getMethods().getNumChild() == 0);

        model.applyTraits();
        assertTrue(cls.getMethods().getNumChild() == 1);
        assertTrue(cls.getMethod(0).getBlock().getStmt(0) instanceof SkipStmt);
    }
    
    @Test
    public void removeExistingMethod()  {
        Model model = assertParseOk(
                "module M; "
                + " trait T = {} removes Unit myMethod();"
                + " class C {adds T;Unit myMethod(){ println(\"\"); } }"
        );

        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertNotNull(cls);
        assertTrue(cls.getMethods().getNumChild() == 1);

        model.applyTraits();
        assertTrue(cls.getMethods().getNumChild() == 0);
    }
    

    @Test(expected=DeltaModellingException.class)
    public void removeNonExistingMethods()  {
        Model model = assertParseOk(
                "module M;"
                + "trait T = {} removes Unit myMethod(); "
                + "class C {adds T;  }"
        );

        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertNotNull(cls);
        assertTrue(cls.getMethods().getNumChild() == 0);

        model.applyTraits();
    }
    

    @Test
    public void addAndRemoveExistingMethod()  {
        Model model = assertParseOk(
                "module M; "
                + " trait T = Unit myMethod(){ skip; } removes Unit myMethod();"
                + " class C {adds T; }"
        );

        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertNotNull(cls);
        assertTrue(cls.getMethods().getNumChild() == 0);

        model.applyTraits();
        assertTrue(cls.getMethods().getNumChild() == 0);
    }
    

    @Test(expected=DeltaModellingException.class)
    public void addRemoveModifyMethod()  {
        Model model = assertParseOk(
                "module M;"
                + "trait T = {Unit myMethod(){ println(\"\"); }} "
                + "trait T2 = {} removes Unit myMethod(); "
                + "trait T3 = {} modifies { Unit myMethod(){ skip; }} "
                + "class C {adds T;adds T2;adds T3;  }"
        );

        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertNotNull(cls);
        assertTrue(cls.getMethods().getNumChild() == 0);

        model.applyTraits();
    }
    
    @Test(expected=DeltaModellingException.class)
    public void circularTraits1()  {
        Model model = assertParseOk(
                "module M;"
                + "trait T = T2 adds{Unit myMethod(){ println(\"\"); }} "
                + "trait T2 = T removes Unit myMethod(); "
                + "class C {adds T2; }"
        );

        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertNotNull(cls);
        assertTrue(cls.getMethods().getNumChild() == 0);

        model.applyTraits();
    }
    

    @Test
    public void addModifyRemoveMethod()  {
        Model model = assertParseOk(
                "module M;"
                + "trait T = {Unit myMethod(){ println(\"\"); }} "
                + "trait T2 = {} removes Unit myMethod(); "
                + "trait T3 = {} modifies { Unit myMethod(){ skip; }} "
                + "class C {adds T;adds T3;adds T2;  }"
        );

        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertNotNull(cls);
        assertTrue(cls.getMethods().getNumChild() == 0);

        model.applyTraits();
        assertTrue(cls.getMethods().getNumChild() == 0);
    }
}
