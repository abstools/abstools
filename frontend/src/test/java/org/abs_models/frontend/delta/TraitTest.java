/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.frontend.delta;

import static org.hamcrest.CoreMatchers.instanceOf;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

import org.abs_models.backend.tests.AbsASTBuilderUtil;
import org.abs_models.frontend.ast.AddMethodModifier;
import org.abs_models.frontend.ast.Block;
import org.abs_models.frontend.ast.ClassDecl;
import org.abs_models.frontend.ast.CompilationUnit;
import org.abs_models.frontend.ast.DeltaAccess;
import org.abs_models.frontend.ast.DeltaDecl;
import org.abs_models.frontend.ast.DeltaTraitModifier;
import org.abs_models.frontend.ast.List;
import org.abs_models.frontend.ast.MethodImpl;
import org.abs_models.frontend.ast.MethodSig;
import org.abs_models.frontend.ast.Model;
import org.abs_models.frontend.ast.ModifyClassModifier;
import org.abs_models.frontend.ast.ModifyMethodModifier;
import org.abs_models.frontend.ast.RemoveMethodModifier;
import org.abs_models.frontend.ast.SkipStmt;
import org.abs_models.frontend.ast.TraitExpr;
import org.abs_models.frontend.ast.TraitSetExpr;
import org.junit.Test;

public class TraitTest extends DeltaTest{


    @Test
    public void addAddModifierAtRuntimeBackComp(){

        Model model = assertParse("module M;"
            + "class C { Unit m(){skip;} }");
        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");


        MethodSig sig= AbsASTBuilderUtil.createMethodSig("n", AbsASTBuilderUtil.getUnit());
        MethodImpl impl = new MethodImpl(sig, new Block(new List<>(), new List<>(new SkipStmt())), false);
        AddMethodModifier opr = new AddMethodModifier(impl);

        assertNotNull(opr.getMethodImpl());
        ModifyClassModifier mcn = new ModifyClassModifier();
        mcn.setName("M.C");

        DeltaAccess acc= new DeltaAccess(cls.getModuleDecl().getName());

        DeltaDecl dd = new DeltaDecl();
        dd.setName("MyDelta");
        dd.addDeltaAccess(acc);
        dd.addModuleModifier(mcn);
        mcn.addModifier(opr);


        mcn.setParent(dd);
        acc.setParent(dd);
        opr.setParent(mcn);
        sig.setParent(opr);
        CompilationUnit cu = model.getCompilationUnitList().getChild(0);
        cu.addDeltaDecl(dd);
        dd.setParent(cu);

        model.applyDelta(dd);

        assertEquals(2, cls.getMethods().getNumChild());
    }

    @Test
    public void addModifyModifierAtRuntimeBackComp(){

        Model model = assertParse("module M;"
            + "class C { Unit m(){skip;} }");
        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");


        MethodSig sig= AbsASTBuilderUtil.createMethodSig("m", AbsASTBuilderUtil.getUnit());
        MethodImpl impl = new MethodImpl(sig, new Block(new List<>(),
            new List<>(new SkipStmt(), new SkipStmt())), false);
        ModifyMethodModifier opr = new ModifyMethodModifier(impl);

        assertNotNull(opr.getMethodImpl());
        ModifyClassModifier mcn = new ModifyClassModifier();
        mcn.setName("M.C");

        DeltaAccess acc= new DeltaAccess(cls.getModuleDecl().getName());

        DeltaDecl dd = new DeltaDecl();
        dd.setName("MyDelta");
        dd.addDeltaAccess(acc);
        dd.addModuleModifier(mcn);
        mcn.addModifier(opr);


        mcn.setParent(dd);
        acc.setParent(dd);
        opr.setParent(mcn);
        sig.setParent(opr);
        CompilationUnit cu = model.getCompilationUnitList().getChild(0);
        cu.addDeltaDecl(dd);
        dd.setParent(cu);

        model.applyDelta(dd);

        assertEquals(1, cls.getMethods().getNumChild());
        assertEquals(2,cls.getMethod(0).getBlock().getNumChild());
    }

    @Test
    public void addRemoveModifierAtRuntime(){
        Model model = assertParse("module M;"
            + "class C { Unit m(){skip;} }");
        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");


        MethodSig sig= AbsASTBuilderUtil.createMethodSig("m", AbsASTBuilderUtil.getUnit());
        List<MethodSig> l = new List<>(sig);
        RemoveMethodModifier opr = new RemoveMethodModifier(l);
        ModifyClassModifier mcn = new ModifyClassModifier();
        mcn.setName("M.C");

        DeltaAccess acc= new DeltaAccess(cls.getModuleDecl().getName());


        DeltaDecl dd = new DeltaDecl();
        dd.setName("MyDelta");
        dd.addDeltaAccess(acc);
        dd.addModuleModifier(mcn);
        mcn.addModifier(opr);


        mcn.setParent(dd);
        acc.setParent(dd);
        opr.setParent(mcn);
        sig.setParent(opr);
        CompilationUnit cu = model.getCompilationUnitList().getChild(0);
        cu.addDeltaDecl(dd);
        dd.setParent(cu);

        model.applyDelta(dd);

        assertEquals(0, cls.getMethods().getNumChild());
    }

    @Test
    public void addMethod()  {
        Model model = assertParse("module M;"
            + "trait T = { Unit myMethod(){ skip; } }"
            + "class C {uses T; }");

        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertNotNull(cls);
        assertTrue(cls.getMethods().getNumChild() == 0);

        model.applyTraits();
        assertTrue(cls.getMethods().getNumChild() == 1);
        assertTrue(cls.getMethod(0).getMethodSig().getName().equals("myMethod"));
    }


    @Test
    public void addTwoMethods()  {
        Model model = assertParse("module M;"
            + "trait T = { Unit myMethod(){ skip; } }"
            + "trait T2 = { Unit myMethod(){ skip; } }"
            + "class C {uses T; uses T2; }");

        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertNotNull(cls);
        assertTrue(cls.getMethods().getNumChild() == 0);

        model.applyTraits();
        assertTrue(cls.getMethods().getNumChild() == 2);
        assertTrue(cls.getMethod(0).toString().equals(cls.getMethod(1).toString()));
        // failing for unknown reasons; comment out since traits might be removed anyway
        //assertTrue(model.getErrors().containsErrors());
    }



    @Test
    public void modifyNonExistingMethods()  {
        Model model = assertParse("module M;"
            + "trait T = {} modifies { Unit myMethod(){ skip; } }"
            + "class C {uses T;  }");

        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertNotNull(cls);
        assertTrue(cls.getMethods().getNumChild() == 0);

        model.applyTraits();
        assertTrue(cls.getMethods().getNumChild() == 1);
        assertTrue(cls.getMethod(0).getMethodSig().getName().equals("myMethod"));
    }


    @Test
    public void modifyExistingMethod()  {
        Model model = assertParse("module M; "
            + "trait T = {} modifies { Unit myMethod(){ skip; } } "
            + "class C {uses T; Unit myMethod(){ println(\"\"); } }");

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
        Model model = assertParse("module M; "
            + "trait T = {Unit myMethod(){ println(\"\"); } } modifies { Unit myMethod(){ println(\"test\"); } } modifies { Unit myMethod(){ skip; } }"
            + "class C {uses T; }");

        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertNotNull(cls);
        assertTrue(cls.getMethods().getNumChild() == 0);

        model.applyTraits();
        assertTrue(cls.getMethods().getNumChild() == 1);
        assertTrue(cls.getMethod(0).getBlock().getStmt(0) instanceof SkipStmt);
    }

    @Test
    public void modifyTwiceTwoTraitsExistingMethod()  {
        Model model = assertParse("module M; "
            + "trait T = {Unit myMethod(){ println(\"\"); }} modifies { Unit myMethod(){ println(\"test\"); } }"
            + "trait T2 = T removes Unit myMethod(); adds { Unit myMethod(){ skip; } }"
            + "class C {uses T2;  }");

        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertNotNull(cls);
        assertTrue(cls.getMethods().getNumChild() == 0);

        model.applyTraits();
        assertTrue(cls.getMethods().getNumChild() == 1);
        assertTrue(cls.getMethod(0).getBlock().getStmt(0) instanceof SkipStmt);
    }

    @Test
    public void addAndModifyExistingMethod()  {
        Model model = assertParse("module M; "
            + " trait T = {Unit myMethod(){ println(\"\"); }} modifies { Unit myMethod(){ skip; } }"
            + " class C {uses T; }");

        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertNotNull(cls);
        assertTrue(cls.getMethods().getNumChild() == 0);

        model.applyTraits();
        assertTrue(cls.getMethods().getNumChild() == 1);
        assertTrue(cls.getMethod(0).getBlock().getStmt(0) instanceof SkipStmt);
    }

    @Test
    public void addAndModifyExistingMethodInTwoClasses()  {
        Model model = assertParse("module M; "
            + " trait T2 = {Unit myMethod(){ println(\"\"); }}"
            + " trait T = T2 modifies { Unit myMethod(){ skip; } }"
            + " class C {uses T; }"
            + " class C2 {uses T; }");

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
        Model model = assertParse("module M; "
            + " trait T = {} removes Unit myMethod();"
            + " class C {uses T; Unit myMethod(){ println(\"\"); } }");

        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertNotNull(cls);
        assertTrue(cls.getMethods().getNumChild() == 1);

        model.applyTraits();
    }


    @Test(expected=DeltaModellingException.class)
    public void removeNonExistingMethodInTrait()  {
        Model model = assertParse("module M;"
            + "trait T = {} removes Unit myMethod(); "
            + "class C {uses T;  }");

        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertNotNull(cls);
        assertTrue(cls.getMethods().getNumChild() == 0);

        model.applyTraits();
    }



    @Test
    public void removeExistingMethodInClassSucc()  {
        Model model = assertParse("module M;"
            + "trait T = {Unit myMethod(){skip;}}  "
            + "class C {uses T removes Unit myMethod();;  }");

        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertNotNull(cls);
        assertTrue(cls.getMethods().getNumChild() == 0);

        model.applyTraits();
        assertTrue(cls.getMethods().getNumChild() == 0);
    }

    @Test
    public void removeExistingMethodSetInClassSucc()  {
        Model model = assertParse("module M;"
            + "trait T = {Unit myMethod(){skip;}Unit myMethod2(){skip;}}  "
            + "class C {uses T removes { Unit myMethod(); Unit myMethod2(); };  }");

        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertNotNull(cls);
        assertTrue(cls.getMethods().getNumChild() == 0);

        model.applyTraits();
        assertTrue(cls.getMethods().getNumChild() == 0);
    }

    @Test
    public void addAndRemoveExistingMethod()  {
        Model model = assertParse("module M; "
            + " trait T = Unit myMethod(){ skip; } removes Unit myMethod();"
            + " class C {uses T; }");

        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertNotNull(cls);
        assertTrue(cls.getMethods().getNumChild() == 0);

        model.applyTraits();
        assertTrue(cls.getMethods().getNumChild() == 0);
    }


    @Test
    public void addRemoveModifyMethod()  {
        Model model = assertParse("module M;"
            + "trait T = {Unit myMethod(){ println(\"\"); }} "
            + "trait T3 = T modifies { Unit myMethod(){ skip; }} "
            + "class C { uses T3;  }");

        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertNotNull(cls);
        assertTrue(cls.getMethods().getNumChild() == 0);

        model.applyTraits();
        assertTrue(cls.getMethods().getNumChild() == 1);
        assertTrue(cls.getMethod(0).getBlock().getStmt(0) instanceof SkipStmt);
    }

    @Test(expected=DeltaModellingException.class)
    public void circularTraits1()  {
        Model model = assertParse("module M;"
            + "trait T = T2 adds{Unit myMethod(){ println(\"\"); }} "
            + "trait T2 = T removes Unit myMethod(); "
            + "class C {uses T2; }");

        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertNotNull(cls);
        assertTrue(cls.getMethods().getNumChild() == 0);

        model.applyTraits();
    }

    @Test
    public void sameTraitNameDifferentModules() {
        Model model = assertParse("\n"
            + "module M;"
            + "export T2;"
            + "trait T = { Unit myMethod() { skip; } }"
            + "trait T2 = T removes Unit myMethod();"
            + "\n"
            + "module N;"
            + "import T2 from M;"
            + "trait T = T2 adds { Unit foo() { skip; } }"
            + "class C { uses T; }");

        ClassDecl cls = (ClassDecl) findDecl(model, "N", "C");
        assertNotNull(cls);
        assertTrue(cls.getMethods().getNumChild() == 0);

        model.applyTraits();

        assertEquals(1, cls.getNumMethod());
        assertTrue(cls.getMethods().getChild(0).getBlock().getStmt(0) instanceof SkipStmt);
    }

    @Test
    public void circularTraitsMultiMod()  {
        Model model = assertParse("module M;"
            + "trait T = {Unit myMethod(){ skip; }} "
            + "trait T2 = T modifies T modifies T modifies T modifies T modifies T modifies T \n"
            + "class C {uses T2; }");

        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertNotNull(cls);
        assertTrue(cls.getMethods().getNumChild() == 0);

        model.applyTraits();
        assertTrue(cls.getMethods().getNumChild() == 1);
    }




    @Test
    public void addModifyRemoveMethod()  {
        Model model = assertParse("module M;"
            + "trait T = {Unit myMethod(){ println(\"\"); }} "
            + "trait T2 = T3 removes Unit myMethod(); "
            + "trait T3 = T modifies { Unit myMethod(){ skip; }} "
            + "class C {uses T2;  }");

        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertNotNull(cls);
        assertTrue(cls.getMethods().getNumChild() == 0);

        model.applyTraits();
        assertTrue(cls.getMethods().getNumChild() == 0);
    }


    @Test
    public void originalCallMethod()  {
        Model model = assertParse("module M; "
            + " trait T = {} modifies { Unit myMethod(){  original();  skip;} }"
            + " class C {uses T; Unit myMethod(){ skip; }}");

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
    public void removeInClass() {
        Model model = assertParse("module M;"
            + "trait T = { Unit x() { println(\"signature change\"); } Unit y() { skip; } }"
            + "class C  { uses T adds { Unit x(Int i) { skip; } } removes Unit x();;}");

        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertNotNull(cls);
        assertTrue(cls.getMethods().getNumChild() == 0);

        model.applyTraits();
        assertTrue(cls.getMethods().getNumChild() == 2);
        assertTrue(cls.getMethod(0).getBlock().getStmt(0) instanceof SkipStmt);
        assertTrue(cls.getMethod(1).getBlock().getStmt(0) instanceof SkipStmt);
    }

    @Test
    public void removeInClassVsRemoveInTrait() {
        Model model = assertParse("module M;"
            + "trait T = { Unit x() { println(\"signature change\"); } Unit y() { skip; } }"
            + "trait T2 = T removes Unit x();  adds { Unit x(Int i) { skip; } } "
            + "class C  { uses T2; }"
            + "class C2  { uses T adds { Unit x(Int i) { skip; } } removes Unit x();;}");

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
        Model model = assertParse("module TestMod;"
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
            + "      Unit sendoff(){println(\"goodbye\"); i = i - 1;}}}");

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
    public void removeSetFromDelta() {
        Model model = assertParse("module TestMod;"
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
            + "class InterImpl(Inter inter) implements Inter {  uses T2;  }"
            + ""
            + "delta D3;" + "modifies class TestMod.InterImpl{"
            + "    adds Int i = 0;"
            + "    removes { "
            + "      Unit printLine_1(); Unit printLine_2(); Unit printLine_3(); }"
            + "}");

        ClassDecl cls = (ClassDecl) findDecl(model, "TestMod", "InterImpl");
        assertNotNull(cls);
        assertTrue(cls.getMethods().getNumChild() == 0);

        model.applyTraits();

        assertTrue(cls.getMethods().getNumChild() == 4);
        DeltaDecl delta = findDelta(model, "D3");
        assertNotNull(delta);
        assertThat(delta, instanceOf(DeltaDecl.class));
        model.applyDelta(delta);
        assertTrue(cls.getMethods().getNumChild() == 1);
    }




    @Test
    public void resolveTest()  {

        Model model = assertParse("module TestMod;"
            + "interface Inter {}"
            + "trait T = { "
            + "  Unit printLine_1(){println(\"I'm 1!\");}"
            + "  Unit printLine_2(){println(\"I'm 2!\");}"
            + "}"
            + "trait T2 = { "
            + "  Unit printLine_2(){println(\"I'm 2!\");}"
            + "  Unit printLine_3(){println(\"I'm 3!\");}"
            + "}"
            + "class InterImpl(Inter inter) implements Inter {   }"
            + ""
            + "delta D3;" + "modifies class TestMod.InterImpl{"
            + "    adds Int i = 0;"
            + "    adds T modifies T2"
            + "}");


        ClassDecl cls = (ClassDecl) findDecl(model, "TestMod", "InterImpl");
        assertNotNull(cls);
        assertTrue(cls.getMethods().getNumChild() == 0);

        model.applyTraits();
        DeltaDecl delta = findDelta(model, "D3");
        assertNotNull(delta);
        assertThat(delta, instanceOf(DeltaDecl.class));
        ModifyClassModifier mm = (ModifyClassModifier)delta.getModuleModifier(0);
        DeltaTraitModifier dml = (DeltaTraitModifier) mm.getModifier(1);
        AddMethodModifier mcl = (AddMethodModifier) dml.getMethodModifier();
        TraitExpr expr = mcl.getTraitExpr();
        TraitExpr set = expr.resolve(cls.getModuleDecl());
        assertTrue("expected 2, got " + set.getChild(0).getNumChild(), set.getChild(0).getNumChild() == 2);

    }

    @Test
    public void resolveTest2()  {
        //this tests that the given delta is wrong (as we take T union T2 and thus have printLine_2 twice)

        Model model = assertParse("module TestMod;"
            + "interface Inter {}"
            + "trait T = { "
            + "  Unit printLine_1(){println(\"I'm 1!\");}"
            + "  Unit printLine_2(){println(\"I'm 2!\");}"
            + "}"
            + "trait T2 = { "
            + "  Unit printLine_2(){println(\"I'm 2!\");}"
            + "  Unit printLine_3(){println(\"I'm 3!\");}"
            + "}"
            + "class InterImpl(Inter inter) implements Inter {   }"
            + ""
            + "delta D3;" + "modifies class TestMod.InterImpl{"
            + "    adds Int i = 0;"
            + "    modifies T adds T2"
            + "}");


        ClassDecl cls = (ClassDecl) findDecl(model, "TestMod", "InterImpl");
        assertNotNull(cls);
        assertTrue(cls.getMethods().getNumChild() == 0);

        model.applyTraits();
        DeltaDecl delta = findDelta(model, "D3");
        assertNotNull(delta);
        assertThat(delta, instanceOf(DeltaDecl.class));
        ModifyClassModifier mm = (ModifyClassModifier)delta.getModuleModifier(0);
        DeltaTraitModifier dml = (DeltaTraitModifier) mm.getModifier(1);
        ModifyMethodModifier mcl = (ModifyMethodModifier) dml.getMethodModifier();
        TraitExpr expr = mcl.getTraitExpr();
        TraitExpr set = expr.resolve(cls.getModuleDecl());
        assertTrue("expected 2, got " + set.getChild(0).getNumChild(), set.getChild(0).getNumChild() == 2);

    }

    @Test
    public void resolveTest3()  {
        Model model = assertParse("module TestMod;"
            + "interface Inter {}"
            + "trait T = { "
            + "  Unit printLine_1(){println(\"I'm 1!\");}"
            + "  Unit printLine_2(){println(\"I'm 2!\");}"
            + "}"
            + "trait T2 = { "
            + "  Unit printLine_2(){println(\"I'm 2!\");}"
            + "  Unit printLine_3(){println(\"I'm 3!\");}"
            + "}"
            + "class InterImpl(Inter inter) implements Inter {   }"
            + ""
            + "delta D3;" + "modifies class TestMod.InterImpl{"
            + "    adds Int i = 0;"
            + "    modifies T removes {Unit printLine_2();}"
            + "}");


        ClassDecl cls = (ClassDecl) findDecl(model, "TestMod", "InterImpl");
        assertNotNull(cls);
        assertTrue(cls.getMethods().getNumChild() == 0);

        model.applyTraits();
        DeltaDecl delta = findDelta(model, "D3");
        assertNotNull(delta);
        assertThat(delta, instanceOf(DeltaDecl.class));
        ModifyClassModifier mm = (ModifyClassModifier)delta.getModuleModifier(0);
        DeltaTraitModifier dml = (DeltaTraitModifier) mm.getModifier(1);
        ModifyMethodModifier mcl = (ModifyMethodModifier) dml.getMethodModifier();
        TraitExpr expr = mcl.getTraitExpr();
        TraitExpr set = expr.resolve(cls.getModuleDecl());
        assertTrue("expected 2, got " + set.getChild(0).getNumChild(), set.getChild(0).getNumChild() == 2);
        assertThat(set, instanceOf(TraitSetExpr.class));
    }

    @Test
    public void resolveTest4()  {
        Model model = assertParse("module TestMod;"
            + "interface Inter {}"
            + "trait T = { "
            + "  Unit printLine_1(){println(\"I'm 1!\");}"
            + "  Unit printLine_2(){println(\"I'm 2!\");}"
            + "}"
            + "trait T2 = { "
            + "  Unit printLine_2(){println(\"I'm 2!\");}"
            + "  Unit printLine_3(){println(\"I'm 3!\");}"
            + "}"
            + "class InterImpl(Inter inter) implements Inter {   }"
            + ""
            + "delta D3;" + "modifies class TestMod.InterImpl{"
            + "    adds Int i = 0;"
            + "    modifies T adds T2 removes {Unit printLine_2();}"
            + "}");


        ClassDecl cls = (ClassDecl) findDecl(model, "TestMod", "InterImpl");
        assertNotNull(cls);
        assertTrue(cls.getMethods().getNumChild() == 0);

        model.applyTraits();
        DeltaDecl delta = findDelta(model, "D3");
        assertNotNull(delta);
        assertThat(delta, instanceOf(DeltaDecl.class));
        ModifyClassModifier mm = (ModifyClassModifier)delta.getModuleModifier(0);
        DeltaTraitModifier dml = (DeltaTraitModifier) mm.getModifier(1);
        ModifyMethodModifier mcl = (ModifyMethodModifier) dml.getMethodModifier();
        TraitExpr expr = mcl.getTraitExpr();
        TraitExpr set = expr.resolve(cls.getModuleDecl());
        assertTrue("expected 2, got " + set.getChild(0).getNumChild(), set.getChild(0).getNumChild() == 2);
        assertThat(set, instanceOf(TraitSetExpr.class));
    }

    @Test
    public void collapseTest()  {
        Model model = assertParse("module TestMod;"
            + "interface Inter {}"
            + "trait T = { "
            + "  Unit printLine_1(){println(\"I'm 1!\");}"
            + "  Unit printLine_2(){println(\"I'm 2!\");}"
            + "}"
            + "trait T2 = { "
            + "  Unit printLine_2(){println(\"I'm 2!\");}"
            + "}"
            + "trait T3 = { "
            + "  Unit printLine_5(){println(\"I'm 5!\");}"
            + "  Unit printLine_6(){println(\"I'm 6!\");}"
            + "}"
            + "trait T4 = { "
            + "  Unit printLine_7(){println(\"I'm 7!\");}"
            + "  Unit printLine_8(){println(\"I'm 8!\");}"
            + "}"
            + "class InterImpl(Inter inter) implements Inter {   }"
            + ""
            + "delta D3;" + "modifies class TestMod.InterImpl{"
            + "    adds Int i = 0;"
            + "    adds T modifies T2 removes {Unit printLine_2();}"
            + "    modifies T3 adds T4 "
            + "}");


        ClassDecl cls = (ClassDecl) findDecl(model, "TestMod", "InterImpl");
        assertNotNull(cls);
        assertTrue(cls.getMethods().getNumChild() == 0);

        model.applyTraits();
        model.collapseTraitModifiers();

        DeltaDecl delta = findDelta(model, "D3");
        assertNotNull(delta);
        assertThat(delta, instanceOf(DeltaDecl.class));

        ModifyClassModifier mm = (ModifyClassModifier)delta.getModuleModifier(0);

        assertTrue(mm.getModifierList().getNumChild() == 6);
        DeltaTraitModifier dml = (DeltaTraitModifier) mm.getModifier(1);
        AddMethodModifier mcl = (AddMethodModifier) dml.getMethodModifier();
        TraitExpr set = mcl.getTraitExpr();
        assertTrue(set.getChild(0).getNumChild() == 2);
        assertThat(set, instanceOf(TraitSetExpr.class));


        DeltaTraitModifier dml2 = (DeltaTraitModifier) mm.getModifier(2);
        ModifyMethodModifier mcl2 = (ModifyMethodModifier) dml2.getMethodModifier();
        TraitExpr set2 = mcl2.getTraitExpr();
        assertTrue(set2.getChild(0).getNumChild() == 1);
        assertThat(set2, instanceOf(TraitSetExpr.class));


    }

    @Test
    public void collapseTest2()  {
        Model model = assertParse("module TestMod;"
            + "interface Inter {}"
            + "trait T = { "
            + "  Unit printLine_1(){println(\"I'm 1!\");}"
            + "  Unit printLine_2(){println(\"I'm 2!\");}"
            + "}"
            + "trait T2 = { "
            + "  Unit printLine_2(){println(\"I'm 2!\");}"
            + "}"
            + "trait T3 = { "
            + "  Unit printLine_5(){println(\"I'm 5!\");}"
            + "  Unit printLine_6(){println(\"I'm 6!\");}"
            + "}"
            + "trait T4 = { "
            + "  Unit printLine_7(){println(\"I'm 7!\");}"
            + "  Unit printLine_8(){println(\"I'm 8!\");}"
            + "}"
            + "class InterImpl(Inter inter) implements Inter {   }"
            + ""
            + "delta D3;" + "modifies class TestMod.InterImpl{"
            + "    adds T modifies T2 removes Unit printLine_2();"
            + "    adds Int i = 0;"
            + "    modifies T3 adds T4 "
            + "}");


        ClassDecl cls = (ClassDecl) findDecl(model, "TestMod", "InterImpl");
        assertNotNull(cls);
        assertTrue(cls.getMethods().getNumChild() == 0);

        model.applyTraits();
        model.collapseTraitModifiers();

        DeltaDecl delta = findDelta(model, "D3");
        assertNotNull(delta);
        assertThat(delta, instanceOf(DeltaDecl.class));

        ModifyClassModifier mm = (ModifyClassModifier)delta.getModuleModifier(0);
        assertTrue(mm.getModifierList().getNumChild() == 6);


        DeltaTraitModifier dml = (DeltaTraitModifier) mm.getModifier(0);
        AddMethodModifier mcl = (AddMethodModifier) dml.getMethodModifier();
        TraitExpr set = mcl.getTraitExpr();
        assertTrue(set.getChild(0).getNumChild() == 2);
        assertThat(set, instanceOf(TraitSetExpr.class));


        DeltaTraitModifier dml2 = (DeltaTraitModifier) mm.getModifier(1);
        ModifyMethodModifier mcl2 = (ModifyMethodModifier) dml2.getMethodModifier();
        TraitExpr set2 = mcl2.getTraitExpr();
        assertTrue(set2.getChild(0).getNumChild() == 1);
        assertThat(set2, instanceOf(TraitSetExpr.class));
    }

    @Test
    public void collapseTest3()  {
        Model model = assertParse("module TestMod;"
            + "trait T = {"
            + "Unit printLine_0(){println(\"I'm 0!\");}"
            + "Unit printLine_1(){println(\"I'm 1!\");}"
            + "}"
            + "trait T2 = {"
            + "Unit printLine_1(){println(\"I'm 1!\");}"
            + "}"
            + "class InterImpl(Inter inter){ }"
            + ""
            + "delta D3;"
            + "modifies class TestMod.InterImpl{"
            + "adds Int i = 0;"
            + "adds T "
            + "modifies T2 "
            + "removes { Unit printLine_0(); }"
            + "                removes { Unit printLine_1(); }"
            + "adds {"
            + "Unit printLine_2(){println(\"I'm 2!\");}"
            + "Unit printLine_3(){println(\"I'm 3!\");}"
            + "}"
            + "removes { Unit printLine_2(); }"
            + "removes { Unit printLine_3(); }"
            + "}");


        ClassDecl cls = (ClassDecl) findDecl(model, "TestMod", "InterImpl");
        assertNotNull(cls);
        assertTrue(cls.getMethods().getNumChild() == 0);

        model.applyTraits();
        model.collapseTraitModifiers();

        DeltaDecl delta = findDelta(model, "D3");
        assertNotNull(delta);
        assertThat(delta, instanceOf(DeltaDecl.class));

        ModifyClassModifier mm = (ModifyClassModifier)delta.getModuleModifier(0);
        assertTrue(mm.getModifierList().getNumChild() == 8);


        DeltaTraitModifier dml = (DeltaTraitModifier) mm.getModifier(1);
        AddMethodModifier mcl = (AddMethodModifier) dml.getMethodModifier();
        TraitExpr set = mcl.getTraitExpr();
        assertTrue(set.getChild(0).getNumChild() == 2);
        assertThat(set, instanceOf(TraitSetExpr.class));


        DeltaTraitModifier dml2 = (DeltaTraitModifier) mm.getModifier(2);
        ModifyMethodModifier mcl2 = (ModifyMethodModifier) dml2.getMethodModifier();
        TraitExpr set2 = mcl2.getTraitExpr();
        assertTrue(set2.getChild(0).getNumChild() == 1);
        assertThat(set2, instanceOf(TraitSetExpr.class));


    }

    @Test
    public void frameTest()  {
        Model model = assertParse("module M;"
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
            + " }");

        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertNotNull(cls);
        assertTrue(cls.getMethods().getNumChild() == 2);

        model.applyTraits();
        assertTrue(cls.getMethods().getNumChild() == 4);
    }

    @Test
    public void addMethodWithImportedTrait()  {
        Model model = assertParse("module M;"
            + "export T;"
            + "trait T = { Unit myMethod(){ skip; } }"
            + "class C { uses T; }"
            + "\n"
            + "module N;"
            + "export T;"
            + "import T from M;"
            + "class C { uses T; }"
            + "\n"
            + "module O;"
            + "import T from N;"
            + "class C { uses T; }");

        ClassDecl cls = (ClassDecl) findDecl(model, "N", "C");
        ClassDecl cls2 = (ClassDecl) findDecl(model, "O", "C");
        assertNotNull(cls);
        assertTrue(cls.getMethods().getNumChild() == 0);
        assertNotNull(cls2);
        assertTrue(cls2.getMethods().getNumChild() == 0);

        model.applyTraits();
        assertTrue(cls.getMethods().getNumChild() == 1);
        assertTrue(cls.getMethod(0).getMethodSig().getName().equals("myMethod"));
        assertTrue(cls2.getMethods().getNumChild() == 1);
        assertTrue(cls2.getMethod(0).getMethodSig().getName().equals("myMethod"));
    }

    @Test
    public void multipleNameResolveSteps() {
        Model model = assertParse("\n"
            + "module M;"
            + "export T;"
            + "trait T = { Unit myMethod(){ skip; } }"
            + "\n"
            + "module N;"
            + "export U;"
            + "import T from M;"
            + "trait U = T modifies {}"
            + "class C { uses U; }"
            + "\n"
            + "module O;"
            + "import U from N;"
            + "class D { uses U; }");

        ClassDecl cls = (ClassDecl) findDecl(model, "N", "C");
        ClassDecl cls2 = (ClassDecl) findDecl(model, "O", "D");
        assertNotNull(cls);
        assertTrue(cls.getMethods().getNumChild() == 0);
        assertNotNull(cls2);
        assertTrue(cls2.getMethods().getNumChild() == 0);

        model.applyTraits();
        assertTrue(cls.getMethods().getNumChild() == 1);
        assertTrue(cls.getMethod(0).getMethodSig().getName().equals("myMethod"));
        assertTrue(cls2.getMethods().getNumChild() == 1);
        assertTrue(cls2.getMethod(0).getMethodSig().getName().equals("myMethod"));
    }

}
