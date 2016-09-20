/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.delta;

import static org.hamcrest.CoreMatchers.instanceOf;
import static org.junit.Assert.*;

import org.junit.Test;

import abs.frontend.ast.ClassDecl;
import abs.frontend.ast.Decl;
import abs.frontend.ast.DeltaDecl;
import abs.frontend.ast.Model;
import abs.frontend.ast.SkipStmt;

public class TraitTest extends DeltaTest{

    @Test
    public void addMethod()  {
        Model model = assertParseOk(
                "module M;"
                + "trait T = { Unit myMethod(){ skip; } }"
                + "class C {uses T; }"
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
                + "class C {uses T; uses T2; }"
        );

        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertNotNull(cls);
        assertTrue(cls.getMethods().getNumChild() == 0);

        model.applyTraits();
        assertTrue(cls.getMethods().getNumChild() == 2);
        assertTrue(cls.getMethod(0).toString().equals(cls.getMethod(1).toString()));
        assertTrue(model.getErrors().containsErrors());
    }

    
    
    @Test
    public void modifyNonExistingMethods()  {
        Model model = assertParseOk(
                "module M;"
                + "trait T = {} modifies { Unit myMethod(){ skip; } }"
                + "class C {uses T;  }"
        );

        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertNotNull(cls);
        assertTrue(cls.getMethods().getNumChild() == 0);

        model.applyTraits();
        assertTrue(cls.getMethods().getNumChild() == 1);
        assertTrue(cls.getMethod(0).getMethodSig().getName().equals("myMethod"));
    }


    @Test
    public void modifyExistingMethod()  {
        Model model = assertParseOk(
                "module M; "
                + "trait T = {} modifies { Unit myMethod(){ skip; } } "
                + "class C {uses T; Unit myMethod(){ println(\"\"); } }"
        );

        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertNotNull(cls);
        assertTrue(cls.getMethods().getNumChild() == 1);

        model.applyTraits();
        assertTrue(cls.getMethods().getNumChild() == 2);
        assertTrue(cls.getMethod(0).getMethodSig().toString().equals(cls.getMethod(1).getMethodSig().toString()));
        //myMethod is added twice
    }
    
    @Test
    public void modifyTwiceExistingMethod()  {
        Model model = assertParseOk(
                "module M; "
                + "trait T = {Unit myMethod(){ println(\"\"); } } modifies { Unit myMethod(){ println(\"test\"); } } modifies { Unit myMethod(){ skip; } }"
                + "class C {uses T; }"
        );

        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertNotNull(cls);
        assertTrue(cls.getMethods().getNumChild() == 0);

        model.applyTraits();
        assertTrue(cls.getMethods().getNumChild() == 1);
        assertTrue(cls.getMethod(0).getBlock().getStmt(0) instanceof SkipStmt);
    }

    @Test
    public void modifyTwiceTwoTraitsExistingMethod()  {
        Model model = assertParseOk(
                "module M; "
                        + "trait T = {Unit myMethod(){ println(\"\"); }} modifies { Unit myMethod(){ println(\"test\"); } }"
                        + "trait T2 = T removes Unit myMethod(); adds { Unit myMethod(){ skip; } }"
                + "class C {uses T2;  }"
        );

        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertNotNull(cls);
        assertTrue(cls.getMethods().getNumChild() == 0);

        model.applyTraits();
        assertTrue(cls.getMethods().getNumChild() == 1);
        assertTrue(cls.getMethod(0).getBlock().getStmt(0) instanceof SkipStmt);
    }

    @Test
    public void addAndModifyExistingMethod()  {
        Model model = assertParseOk(
                "module M; "
                + " trait T = {Unit myMethod(){ println(\"\"); }} modifies { Unit myMethod(){ skip; } }"
                + " class C {uses T; }"
        );

        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertNotNull(cls);
        assertTrue(cls.getMethods().getNumChild() == 0);

        model.applyTraits();
        assertTrue(cls.getMethods().getNumChild() == 1);
        assertTrue(cls.getMethod(0).getBlock().getStmt(0) instanceof SkipStmt);
    }    
    
    @Test
    public void addAndModifyExistingMethodInTwoClasses()  {
        Model model = assertParseOk(
                "module M; "
                + " trait T2 = {Unit myMethod(){ println(\"\"); }}"
                + " trait T = T2 modifies { Unit myMethod(){ skip; } }"
                + " class C {uses T; }"
                + " class C2 {uses T; }"
        );

        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertNotNull(cls);
        assertTrue(cls.getMethods().getNumChild() == 0);
        ClassDecl cls2 = (ClassDecl) findDecl(model, "M", "C2");
        assertNotNull(cls2);
        assertTrue(cls2.getMethods().getNumChild() == 0);

        model.applyTraits();
        assertTrue(cls.getMethods().getNumChild() == 1);
        assertTrue(cls.getMethod(0).getBlock().getStmt(0) instanceof SkipStmt);
        assertTrue(cls2.getMethods().getNumChild() == 1);
        assertTrue(cls2.getMethod(0).getBlock().getStmt(0) instanceof SkipStmt);
    }

    @Test(expected=DeltaModellingException.class)
    public void removeExistingMethodInTrait()  {
        Model model = assertParseOk(
                "module M; "
                + " trait T = {} removes Unit myMethod();"
                + " class C {uses T; Unit myMethod(){ println(\"\"); } }"
        );

        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertNotNull(cls);
        assertTrue(cls.getMethods().getNumChild() == 1);

        model.applyTraits();
    }


    @Test(expected=DeltaModellingException.class)
    public void removeNonExistingMethodInTrait()  {
        Model model = assertParseOk(
                "module M;"
                + "trait T = {} removes Unit myMethod(); "
                + "class C {uses T;  }"
        );

        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertNotNull(cls);
        assertTrue(cls.getMethods().getNumChild() == 0);

        model.applyTraits();
    }
    


    @Test
    public void removeExistingMethodInClassSucc()  {
        Model model = assertParseOk(
                "module M;"
                + "trait T = {Unit myMethod(){skip;}}  "
                + "class C {uses T removes Unit myMethod();;  }"
        );

        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertNotNull(cls);
        assertTrue(cls.getMethods().getNumChild() == 0);

        model.applyTraits();
        assertTrue(cls.getMethods().getNumChild() == 0);
    }

    @Test
    public void addAndRemoveExistingMethod()  {
        Model model = assertParseOk(
                "module M; "
                + " trait T = Unit myMethod(){ skip; } removes Unit myMethod();"
                + " class C {uses T; }"
        );

        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertNotNull(cls);
        assertTrue(cls.getMethods().getNumChild() == 0);

        model.applyTraits();
        assertTrue(cls.getMethods().getNumChild() == 0);
    }
    

    @Test
    public void addRemoveModifyMethod()  {
        Model model = assertParseOk(
                "module M;"
                + "trait T = {Unit myMethod(){ println(\"\"); }} "
                + "trait T3 = T modifies { Unit myMethod(){ skip; }} "
                + "class C { uses T3;  }"
        );

        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertNotNull(cls);
        assertTrue(cls.getMethods().getNumChild() == 0);

        model.applyTraits();
        assertTrue(cls.getMethods().getNumChild() == 1);
        assertTrue(cls.getMethod(0).getBlock().getStmt(0) instanceof SkipStmt);
    }
    
    @Test(expected=DeltaModellingException.class)
    public void circularTraits1()  {
        Model model = assertParseOk(
                "module M;"
                + "trait T = T2 adds{Unit myMethod(){ println(\"\"); }} "
                + "trait T2 = T removes Unit myMethod(); "
                + "class C {uses T2; }"
        );

        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertNotNull(cls);
        assertTrue(cls.getMethods().getNumChild() == 0);

        model.applyTraits();
    }
    
    @Test
    public void circularTraits()  {
        Model model = assertParseOk(
                "module M;"
                + "trait T = {Unit myMethod(){ skip; }} "
                + "trait T2 = T modifies T modifies T modifies T modifies T modifies T modifies T \n"
                + "class C {uses T2; }"
        );

        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertNotNull(cls);
        assertTrue(cls.getMethods().getNumChild() == 0);

        model.applyTraits();
        assertTrue(cls.getMethods().getNumChild() == 1);
    }
    

    @Test
    public void addModifyRemoveMethod()  {
        Model model = assertParseOk(
                "module M;"
                + "trait T = {Unit myMethod(){ println(\"\"); }} "
                + "trait T2 = T3 removes Unit myMethod(); "
                + "trait T3 = T modifies { Unit myMethod(){ skip; }} "
                + "class C {uses T2;  }"
        );

        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertNotNull(cls);
        assertTrue(cls.getMethods().getNumChild() == 0);

        model.applyTraits();
        assertTrue(cls.getMethods().getNumChild() == 0);
    }
    

    @Test
    public void originalCallMethod()  {
        Model model = assertParseOk(
                "module M; "
                + " trait T = {} modifies { Unit myMethod(){  original();  skip;} }"
                + " class C {uses T; Unit myMethod(){ skip; }}"
        );

        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertNotNull(cls);
        assertTrue(cls.getMethods().getNumChild() == 1);

        model.applyTraits();
        assertTrue(cls.getMethods().getNumChild() == 2);
        assertFalse(cls.getMethod(1).getBlock().getStmt(0) instanceof SkipStmt);
        assertTrue(cls.getMethod(1).getBlock().getStmt(1) instanceof SkipStmt);
        assertTrue(cls.getMethod(0).getBlock().getStmt(0) instanceof SkipStmt);
        assertFalse(cls.getMethod(0).toString().equals(cls.getMethod(1).toString()));
    }
    
    @Test
    public void removeInClassVsRemoveInTrait()  {
        Model model = assertParseOk(
                "module M;"
                +"trait T = { Unit x() { println(\"signature change\"); } Unit y() { skip; } }"
                +"trait T2 = T removes Unit x();  adds { Unit x(Int i) { skip; } } "
                +"class C  { uses T2; }"
                +"class C2  { uses T adds { Unit x(Int i) { skip; } } removes Unit x();;}"

        );

        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertNotNull(cls);
        assertTrue(cls.getMethods().getNumChild() == 0);
        ClassDecl cls2 = (ClassDecl) findDecl(model, "M", "C2");
        assertNotNull(cls2);
        assertTrue(cls2.getMethods().getNumChild() == 0);

        model.applyTraits();
        assertTrue(cls.getMethods().getNumChild() == 2);
        assertTrue(cls.getMethod(0).getBlock().getStmt(0) instanceof SkipStmt);
        assertTrue(cls.getMethod(1).getBlock().getStmt(0) instanceof SkipStmt);
        assertTrue(cls2.getMethods().getNumChild() == 2);
        assertTrue(cls2.getMethod(0).getBlock().getStmt(0) instanceof SkipStmt);
        assertTrue(cls2.getMethod(1).getBlock().getStmt(0) instanceof SkipStmt);
    }

    @Test
    public void addSameMethodsTwice() {
        Model model = assertParseOk("module TestMod;" 
                + "interface Inter {}" 
                + "trait T2 = { " 
                + "  Unit driver(){"
                + "    println(\"hallo\");" 
                + "    this.greeting();" 
                + "    this.printLine_1();" 
                + "    this.printLine_2();"
                + "    this.sendoff();" 
                + "  }" 
                + "  Unit printLine_1(){println(\"I'm 1!\");}"
                + "  Unit printLine_2(){println(\"I'm 2!\");}" 
                + "  Unit printLine_3(){println(\"I'm 3!\");}"
                + "}"
                + "class InterImpl(Inter inter) implements Inter {  uses T2 removes Unit printLine_3();; }"
                + "class InterImpl2(Inter inter) implements Inter { uses T2 removes Unit printLine_3();; }"
                + ""
                + "delta D3;" + "modifies class TestMod.InterImpl{" 
                + "    adds Int i = 0;" 
                + "    adds { "
                + "      Unit greeting(){println(\"hello\"); i = i + 1;} "
                + "      Unit sendoff(){println(\"goodbye\"); i = i - 1;}}}"
                + "modifies class TestMod.InterImpl2{" + 
                "      adds Int i = 0;" + 
                "      adds { "
                + "      Unit greeting(){println(\"hello\"); i = i + 1;} "
                + "      Unit sendoff(){println(\"goodbye\"); i = i - 1;} }}");

        ClassDecl cls = (ClassDecl) findDecl(model, "TestMod", "InterImpl");
        assertNotNull(cls);
        assertTrue(cls.getMethods().getNumChild() == 0);
        ClassDecl cls2 = (ClassDecl) findDecl(model, "TestMod", "InterImpl2");
        assertNotNull(cls2);
        assertTrue(cls2.getMethods().getNumChild() == 0);

        model.applyTraits();

        assertTrue(cls.getMethods().getNumChild() == 3);
        assertTrue(cls2.getMethods().getNumChild() == 3);
        DeltaDecl delta = findDelta(model, "D3");
        assertNotNull(delta);
        assertThat(delta, instanceOf(DeltaDecl.class));
        model.applyDelta(delta);
        assertTrue(cls.getMethods().getNumChild() == 5);
        assertTrue(cls2.getMethods().getNumChild() == 5);
    }
    

    @Test
    public void frameTest()  {
        Model model = assertParseOk(
                "module M;"
                + " interface I { Unit x(); Unit foo(); Unit bar(); }"
                + " trait T = Unit x() { this.foo(); original(); this.bar();  }"
                + " trait T2 = { Unit x() { println(\"T2\"); } } modifies T"
                + " trait T3 = { Unit x() { println(\"T3\"); } } modifies T"
                + " class C implements I {"
                + "         Int i = 0;"
                + "         uses T2;"
                + "         Unit foo(){ i = i+1; }"
                + "         Unit bar(){ i = i-1; }"
                + " }"
                + " class C2 implements I {"
                + "         Int i = 0;"
                + "         uses T3;"
                + "         Unit foo(){ i = i-1; }"
                + "         Unit bar(){ i = i+1; }"
                + " }"
        );

        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertNotNull(cls);
        assertTrue(cls.getMethods().getNumChild() == 2);

        model.applyTraits();
        assertTrue(cls.getMethods().getNumChild() == 4);
    }
    
}
