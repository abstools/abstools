package org.abs_models.frontend.delta.localpls;

import org.abs_models.common.WrongProgramArgumentException;
import org.abs_models.frontend.analyser.ErrorMessage;
import org.abs_models.frontend.analyser.SemanticConditionList;
import org.abs_models.frontend.ast.*;
import org.abs_models.frontend.delta.DeltaTest;
import org.abs_models.frontend.typechecker.UnionType;
import org.junit.Assert;
import org.junit.Test;

import static org.junit.Assert.*;

public class ReferencesUpdateTest extends DeltaTest {

    String vm = "module M;"
        + "export *;"
        + "configuration P1 = {A};"
        + "configuration P2 = {B};"
        + "interface I{}"
        + "unique interface I2 {"
        + " Bool m2();"
        + "}"
        + "class C () implements I { "
        + "}"
        + "class C2 () implements I2 {"
        + "Bool m2() {"
        + "  return True;"
        + "}"
        + "} "
        + "unique class C3 () implements I2 { "
        + "Bool m2() {"
        + "  return False;"
        + "}"
        + "}"
        + "features A, B;"
        + "delta DA;"
        + "modifies interface I {"
        + " adds Unit me1(); }"
        + "modifies class C {"
        + " adds Unit me1(){"
        + "    println(\"S\");"
        + "} }"
        + "delta DB;"
        + "modifies interface I {"
        + " adds Unit me3(); }"
        + "modifies class C {"
        + " adds Unit me3(){"
        + "    println(\"L\");"
        + "} }"
        + "modifies class C2 {"
        + " adds Unit me3(){"
        + "    println(\"S\");"
        + "} }"
        + "delta DA when A;"
        + "delta DB when B;";


    @Test
    public void ituMainBlockBase() throws WrongProgramArgumentException {
        Model model = assertParse(
            vm
                + "module TestMain;"
                + "import * from M;"
                + " { I with P1 m; }"
        );
        model.flattenforLocalProducts();
        assertEquals(0, model.getTypeErrors().getErrorCount());

        ModuleDecl testModule = model.lookupModule("TestMain");
        assertNotNull(testModule.getBlock());
        assertEquals(1, testModule.getBlock().getNumStmt());
        VarDecl var = ((VarDeclStmt) testModule.getBlock().getStmt(0)).getVarDecl();
        assertTrue(var.getAccess() instanceof InterfaceTypeUse);
        assertFalse(var.getAccess() instanceof VariableInterfaceTypeUse);
        assertEquals("M_A.I", ((InterfaceTypeUse) var.getAccess()).getName());
        InterfaceDecl interface1 = (InterfaceDecl) findDecl(model, "M_A", "I");
        assertEquals(interface1,((InterfaceTypeUse) var.getAccess()).getDecl());
    }

    @Test
    public void ituMainBlock() throws WrongProgramArgumentException {
        Model model = assertParse(
            vm
                + "module TestMain;"
                + "import * from M;"
                + " { I with P1 m = new C(); }"
                
        );
        model.flattenforLocalProducts();
        assertEquals(0, model.getTypeErrors().getErrorCount());

        ModuleDecl testModule = model.lookupModule("TestMain");
        assertNotNull(testModule.getBlock());
        assertEquals(1, testModule.getBlock().getNumStmt());
        VarDecl var = ((VarDeclStmt) testModule.getBlock().getStmt(0)).getVarDecl();
        assertTrue(var.getAccess() instanceof InterfaceTypeUse);
        assertFalse(var.getAccess() instanceof VariableInterfaceTypeUse);
        assertEquals("M_A.I", ((InterfaceTypeUse) var.getAccess()).getName());
        InterfaceDecl interface1 = (InterfaceDecl) findDecl(model, "M_A", "I");
        assertEquals(interface1,((InterfaceTypeUse) var.getAccess()).getDecl());
        assertTrue(((VarDeclStmt) testModule.getBlock().getStmt(0)).getVarDecl().hasInitExp());

        ClassDecl class1 = (ClassDecl) findDecl(model, "M_A", "C");
        NewExp ne = (NewExp) ((VarDeclStmt) testModule.getBlock().getStmt(0)).getVarDecl().getInitExp();
        assertEquals("M_A.C",ne.getClassName());
        assertEquals(class1, ((UnionType) ne.getType()).getOriginatingClass());
    }

    @Test
    public void mainBlockNotBase() throws WrongProgramArgumentException {
        Model model = assertParse(
            vm
                + "module TestMain;"
                + "import * from M;"
                + " { I2 with P2 m = new C2(); }"
                
        );
        model.flattenforLocalProducts();
        assertEquals(0, model.getTypeErrors().getErrorCount());

        ModuleDecl testModule = model.lookupModule("TestMain");
        assertNotNull(testModule.getBlock());
        assertEquals(1, testModule.getBlock().getNumStmt());
        VarDecl var = ((VarDeclStmt) testModule.getBlock().getStmt(0)).getVarDecl();
        assertTrue(var.getAccess() instanceof InterfaceTypeUse);
        assertFalse(var.getAccess() instanceof VariableInterfaceTypeUse);
        assertEquals("I2", ((InterfaceTypeUse) var.getAccess()).getName());
        InterfaceDecl interface1 = (InterfaceDecl) findDecl(model, "M", "I2");
        assertEquals(interface1,((InterfaceTypeUse) var.getAccess()).getDecl());
        assertTrue(((VarDeclStmt) testModule.getBlock().getStmt(0)).getVarDecl().hasInitExp());

        ClassDecl class1 = (ClassDecl) findDecl(model, "M_B", "C2");
        NewExp ne = (NewExp) ((VarDeclStmt) testModule.getBlock().getStmt(0)).getVarDecl().getInitExp();
        assertEquals("M_B.C2",ne.getClassName());
        assertEquals(class1, ((UnionType) ne.getType()).getOriginatingClass());
    }

    @Test
    public void ituInVarDeclNoBase() throws WrongProgramArgumentException {
        Model model = assertParse(
            vm
                + "module TestMain;"
                + "import * from M;"
                + " { I2 with P2 m = new C2(); }"
                
        );
        model.flattenforLocalProducts();
        assertEquals(0, model.getTypeErrors().getErrorCount());

        ModuleDecl testModule = model.lookupModule("TestMain");
        assertNotNull(testModule.getBlock());
        assertEquals(1, testModule.getBlock().getNumStmt());
        VarDecl var = ((VarDeclStmt) testModule.getBlock().getStmt(0)).getVarDecl();
        assertTrue(var.getAccess() instanceof InterfaceTypeUse);
        assertFalse(var.getAccess() instanceof VariableInterfaceTypeUse);
        assertEquals("I2", ((InterfaceTypeUse) var.getAccess()).getName());
        InterfaceDecl interface1 = (InterfaceDecl) findDecl(model, "M", "I2");
        assertEquals(interface1,((InterfaceTypeUse) var.getAccess()).getDecl());
        assertTrue(((VarDeclStmt) testModule.getBlock().getStmt(0)).getVarDecl().hasInitExp());

        ClassDecl class1 = (ClassDecl) findDecl(model, "M_B", "C2");
        NewExp ne = (NewExp) ((VarDeclStmt) testModule.getBlock().getStmt(0)).getVarDecl().getInitExp();
        assertEquals("M_B.C2",ne.getClassName());
        assertEquals(class1, ((UnionType) ne.getType()).getOriginatingClass());
    }
    @Test
    public void extendedInterfaceNotBase() throws WrongProgramArgumentException {
        Model model = assertParse(
            vm
                + "module TestMain;"
                + "import * from M;"
                + "interface I3 extends I2 with {A} {}"
        );
        model.flattenforLocalProducts();
        assertEquals(0, model.getTypeErrors().getErrorCount());

        InterfaceDecl interface1 = (InterfaceDecl) findDecl(model, "TestMain", "I3");
        assertNotNull(interface1);

        assertTrue(interface1.getExtendedInterfaceUse(0) instanceof InterfaceTypeUse);
        assertTrue(!(interface1.getExtendedInterfaceUse(0) instanceof VariableInterfaceTypeUse));
        assertEquals("I2", interface1.getExtendedInterfaceUse(0).getName());
    }

    @Test
    public void extendedInterface() throws WrongProgramArgumentException {
        Model model = assertParse(
            vm
                + "module TestMain;"
                + "import * from M;"
                + "interface I3 extends I with {B} {}"
                + "interface I4 extends I with {A} {}"
                + "interface I5 extends I2 with P1 {}"
            
        );
        model.flattenforLocalProducts();
        assertEquals(0, model.getTypeErrors().getErrorCount());

        InterfaceDecl interface3 = (InterfaceDecl) findDecl(model, "TestMain", "I3");
        assertNotNull(interface3);
        assertTrue(interface3.getExtendedInterfaceUse(0) instanceof InterfaceTypeUse);
        assertTrue(!(interface3.getExtendedInterfaceUse(0) instanceof VariableInterfaceTypeUse));
        assertEquals("M_B.I", interface3.getExtendedInterfaceUse(0).getName());
        InterfaceDecl iB = (InterfaceDecl) findDecl(model, "M_B", "I");
        assertNotNull(iB);
        assertEquals(iB, interface3.getExtendedInterfaceUse(0).getDecl());

        InterfaceDecl interface4 = (InterfaceDecl) findDecl(model, "TestMain", "I4");
        assertNotNull(interface4);
        assertTrue(interface4.getExtendedInterfaceUse(0) instanceof InterfaceTypeUse);
        assertTrue(!(interface4.getExtendedInterfaceUse(0) instanceof VariableInterfaceTypeUse));
        assertEquals("M_A.I", interface4.getExtendedInterfaceUse(0).getName());
        InterfaceDecl iA = (InterfaceDecl) findDecl(model, "M_A", "I");
        assertNotNull(iA);
        assertEquals(iA, interface4.getExtendedInterfaceUse(0).getDecl());

        InterfaceDecl interface5 = (InterfaceDecl) findDecl(model, "TestMain", "I5");
        assertNotNull(interface5);
        assertTrue(interface5.getExtendedInterfaceUse(0) instanceof InterfaceTypeUse);
        assertTrue(!(interface5.getExtendedInterfaceUse(0) instanceof VariableInterfaceTypeUse));
        assertEquals("I2", interface5.getExtendedInterfaceUse(0).getName());
        InterfaceDecl i = (InterfaceDecl) findDecl(model, "M", "I2");
        assertNotNull(i);
        assertEquals(i, interface5.getExtendedInterfaceUse(0).getDecl());
    }

    @Test
    public void implementsInterface() throws WrongProgramArgumentException {
        Model model = assertParse(
            vm
                + "module TestMain;"
                + "import * from M;"
                + "class C4 () implements I with {B} { "
                + " Unit me3() { println(\"C3\"); }"
                + "}"
                + "class C5 () implements I2 with P1 { "
                + "Bool m2() {"
                + "  return True;"
                + "}"
                + " }"
                
        );
        model.flattenforLocalProducts();
        assertEquals(0, model.getTypeErrors().getErrorCount());
        ClassDecl class4 = (ClassDecl) findDecl(model, "TestMain", "C4");
        assertNotNull(class4);
        assertTrue(class4.getImplementedInterfaceUse(0) instanceof InterfaceTypeUse);
        assertTrue(!(class4.getImplementedInterfaceUse(0) instanceof VariableInterfaceTypeUse));
        assertEquals("M_B.I", class4.getImplementedInterfaceUse(0).getName());
        InterfaceDecl iB = (InterfaceDecl) findDecl(model, "M_B", "I");
        assertNotNull(iB);
        assertEquals(iB, class4.getImplementedInterfaceUse(0).getDecl());

        ClassDecl class5 = (ClassDecl) findDecl(model, "TestMain", "C5");
        assertNotNull(class5);
        assertTrue(class5.getImplementedInterfaceUse(0) instanceof InterfaceTypeUse);
        assertTrue(!(class5.getImplementedInterfaceUse(0) instanceof VariableInterfaceTypeUse));
        assertEquals("I2", class5.getImplementedInterfaceUse(0).getName());
        InterfaceDecl i = (InterfaceDecl) findDecl(model, "M", "I2");
        assertNotNull(i);
        assertEquals(i, class5.getImplementedInterfaceUse(0).getDecl());
    }

    @Test
    public void classParamAndVar() throws WrongProgramArgumentException {
        Model model = assertParse(
            vm
                + "module TestMain;"
                + "import * from M;"
                + "class C4 (I with P1 p1) { "
                + "  I v1 = p1;"
                + "}"
                + "class C5 (I2 with P2 p) { "
                + "I2 p2 = p;"
                + " }"
                
        );
        model.flattenforLocalProducts();
        assertEquals(0, model.getTypeErrors().getErrorCount());
        ClassDecl class4 = (ClassDecl) findDecl(model, "TestMain", "C4");
        assertNotNull(class4);
        assertTrue(class4.getParam(0).getAccess() instanceof InterfaceTypeUse);
        assertTrue(!(class4.getParam(0).getAccess() instanceof VariableInterfaceTypeUse));
        assertEquals("M_A.I", ((InterfaceTypeUse) class4.getParam(0).getAccess()).getName());
        InterfaceDecl iA = (InterfaceDecl) findDecl(model, "M_A", "I");
        assertNotNull(iA);
        assertEquals(iA, ((InterfaceTypeUse) class4.getParam(0).getAccess()).getDecl());
        assertTrue(class4.getField(0).getAccess() instanceof InterfaceTypeUse);
        assertEquals("M_A.I", ((InterfaceTypeUse) class4.getField(0).getAccess()).getName());
        assertEquals(iA, ((InterfaceTypeUse) class4.getField(0).getAccess()).getDecl());


        ClassDecl class5 = (ClassDecl) findDecl(model, "TestMain", "C5");
        assertNotNull(class5);
        assertTrue(class5.getParam(0).getAccess() instanceof InterfaceTypeUse);
        assertTrue(!(class5.getParam(0).getAccess()instanceof VariableInterfaceTypeUse));
        assertEquals("I2", ((InterfaceTypeUse) class5.getParam(0).getAccess()).getName());
        InterfaceDecl i = (InterfaceDecl) findDecl(model, "M", "I2");
        assertNotNull(i);
        assertEquals(i, ((InterfaceTypeUse) class5.getParam(0).getAccess()).getDecl());
        assertTrue(class5.getField(0).getAccess() instanceof InterfaceTypeUse);
        assertEquals("I2", ((InterfaceTypeUse) class5.getField(0).getAccess()).getName());
        assertEquals(i, ((InterfaceTypeUse) class5.getField(0).getAccess()).getDecl());
    }
    @Test
    public void classFieldAndMethod() throws WrongProgramArgumentException {
        Model model = assertParse(
            vm
                + "module TestMain;"
                + "import * from M;"
                + "class C4 () { "
                + "  I with P2 v1;"
                + "  Unit setV1() {"
                + "    v1 = new C ();"
                + "  }"
                + "}"
                + "class C5 () { "
                + "  I2 with P1 v1;"
                + "  Unit setV1() {"
                + "    v1 = new C2 ();"
                + "  }"
                + " }"
                
        );
        model.flattenforLocalProducts();
        assertEquals(0, model.getTypeErrors().getErrorCount());
        ClassDecl class4 = (ClassDecl) findDecl(model, "TestMain", "C4");
        assertNotNull(class4);
        assertTrue(class4.getField(0).getAccess() instanceof InterfaceTypeUse);
        assertTrue(!(class4.getField(0).getAccess() instanceof VariableInterfaceTypeUse));
        assertEquals("M_B.I", ((InterfaceTypeUse) class4.getField(0).getAccess()).getName());
        InterfaceDecl iB = (InterfaceDecl) findDecl(model, "M_B", "I");
        assertNotNull(iB);
        assertEquals(iB, ((InterfaceTypeUse) class4.getField(0).getAccess()).getDecl());
        assertTrue(class4.getMethod(0).getBlock().getStmt(0) instanceof AssignStmt);
        assertTrue(((AssignStmt)class4.getMethod(0).getBlock().getStmt(0) ).getValue() instanceof NewExp);
        NewExp ne = (NewExp)((AssignStmt)class4.getMethod(0).getBlock().getStmt(0) ).getValue();
        ClassDecl c = (ClassDecl) findDecl(model, "M_B", "C");
        assertEquals("M_B.C" ,ne.getClassName());
        assertEquals(c, ((UnionType) ne.getType()).getOriginatingClass());

        ClassDecl class5 = (ClassDecl) findDecl(model, "TestMain", "C5");
        assertNotNull(class5);
        assertTrue(class5.getField(0).getAccess() instanceof InterfaceTypeUse);
        assertTrue(!(class5.getField(0).getAccess() instanceof VariableInterfaceTypeUse));
        assertEquals("I2", ((InterfaceTypeUse) class5.getField(0).getAccess()).getName());
        InterfaceDecl i = (InterfaceDecl) findDecl(model, "M", "I2");
        assertNotNull(i);
        assertEquals(i, ((InterfaceTypeUse) class5.getField(0).getAccess()).getDecl());
        assertTrue(class5.getMethod(0).getBlock().getStmt(0) instanceof AssignStmt);
        assertTrue(((AssignStmt)class5.getMethod(0).getBlock().getStmt(0) ).getValue() instanceof NewExp);
        NewExp ne2 = (NewExp)((AssignStmt)class5.getMethod(0).getBlock().getStmt(0) ).getValue();
        ClassDecl c2 = (ClassDecl) findDecl(model, "M_A", "C2");
        assertNotNull(c2);
        assertEquals("M_A.C2" ,ne2.getClassName());
        assertEquals(c2, ((UnionType) ne2.getType()).getOriginatingClass());

    }
    @Test
    public void methodVars() throws WrongProgramArgumentException {
        Model model = assertParse(
            vm
                + "module TestMain;"
                + "import * from M;"
                + "class C4 () { "
                + "  Unit setV1() {"
                + "  I with P2 v1;"
                + "    v1 = new C ();"
                + "  I with P1 v2 = new C ();"
                + "  I2 with P2 v3 = new C2 ();"
                + "  I2 with P2 v4 = new C3 ();"
                + "  }"
                + "}"
                
        );
        model.flattenforLocalProducts();
        assertEquals(0, model.getTypeErrors().getErrorCount());
        ClassDecl class4 = (ClassDecl) findDecl(model, "TestMain", "C4");
        assertNotNull(class4);
        InterfaceDecl iB = (InterfaceDecl) findDecl(model, "M_B", "I");
        assertNotNull(iB);

        assertTrue(class4.getMethod(0).getBlock().getStmt(1) instanceof AssignStmt);
        assertTrue(((AssignStmt)class4.getMethod(0).getBlock().getStmt(1) ).getVar().getDecl() instanceof  VarDecl);
        VarDecl var =  (VarDecl) ((AssignStmt)class4.getMethod(0).getBlock().getStmt(1)).getVar().getDecl();
        assertTrue(var.getAccess() instanceof InterfaceTypeUse);
        assertEquals("M_B.I", ((InterfaceTypeUse) var.getAccess()).getName());
        assertTrue(((AssignStmt)class4.getMethod(0).getBlock().getStmt(1) ).getValue() instanceof NewExp);
        NewExp ne = (NewExp)((AssignStmt)class4.getMethod(0).getBlock().getStmt(1) ).getValue();
        ClassDecl cB = (ClassDecl) findDecl(model, "M_B", "C");
        assertEquals("M_B.C" ,ne.getClassName());
        assertEquals(cB, ((UnionType) ne.getType()).getOriginatingClass());

        assertTrue(class4.getMethod(0).getBlock().getStmt(2) instanceof VarDeclStmt);
        VarDecl var2 = ((VarDeclStmt)class4.getMethod(0).getBlock().getStmt(2)).getVarDecl();
        assertTrue(var2.getAccess() instanceof InterfaceTypeUse);
        assertEquals("M_A.I", ((InterfaceTypeUse) var2.getAccess()).getName());
        assertTrue(var2.getInitExp() instanceof NewExp);
        NewExp ne2 = (NewExp) var2.getInitExp();
        ClassDecl cA = (ClassDecl) findDecl(model, "M_A", "C");
        assertEquals("M_A.C" ,ne2.getClassName());
        assertEquals(cA, ((UnionType) ne2.getType()).getOriginatingClass());

        assertTrue(class4.getMethod(0).getBlock().getStmt(3) instanceof VarDeclStmt);
        VarDecl var3 = ((VarDeclStmt)class4.getMethod(0).getBlock().getStmt(3)).getVarDecl();
        assertTrue(var3.getAccess() instanceof InterfaceTypeUse);
        assertEquals("I2", ((InterfaceTypeUse) var3.getAccess()).getName());
        assertTrue(var3.getInitExp() instanceof NewExp);
        NewExp ne3 = (NewExp) var3.getInitExp();
        ClassDecl c2 = (ClassDecl) findDecl(model, "M_B", "C2");
        assertNotNull(c2);
        assertEquals("M_B.C2" ,ne3.getClassName());
        assertEquals(c2, ((UnionType) ne3.getType()).getOriginatingClass());

        assertTrue(class4.getMethod(0).getBlock().getStmt(4) instanceof VarDeclStmt);
        VarDecl var4 = ((VarDeclStmt)class4.getMethod(0).getBlock().getStmt(4)).getVarDecl();
        assertTrue(var4.getAccess() instanceof InterfaceTypeUse);
        assertEquals("I2", ((InterfaceTypeUse) var4.getAccess()).getName());
        assertTrue(var3.getInitExp() instanceof NewExp);
        NewExp ne4 = (NewExp) var4.getInitExp();
        ClassDecl c3 = (ClassDecl) findDecl(model, "M", "C3");
        assertNotNull(c3);
        assertEquals("C3" ,ne4.getClassName());
        assertEquals(c3, ((UnionType) ne4.getType()).getOriginatingClass());
    }

    @Test
    public void methodVars2() throws WrongProgramArgumentException {
        Model model = assertParse(
            vm
                + "module TestMain;"
                + "import * from M;"
                + "class C4 () { "
                + "  Unit setV1(I with {A} p1, I p2, I2 p3, I2 p4 ) {"
                + "    p1 = new C ();"
                + "    p2  = new C () with {B};"
                + "    p3 = new C2 () with P1;"
                + "    p4  = new C3 ();"
                + "  }"
                + "}"
                
        );
        model.flattenforLocalProducts();
        SemanticConditionList errors = model.getTypeErrors();
        assertEquals(0, model.getTypeErrors().getErrorCount());
        ClassDecl class4 = (ClassDecl) findDecl(model, "TestMain", "C4");
        assertNotNull(class4);

        MethodSig methodSig = class4.getMethod(0).getMethodSig();
        assertTrue(methodSig.getParam(0).getAccess() instanceof InterfaceTypeUse);
        assertTrue(! (methodSig.getParam(0).getAccess() instanceof VariableInterfaceTypeUse));

        Stmt stmt = class4.getMethod(0).getBlock().getStmt(0);
        assertTrue( stmt instanceof AssignStmt);
        assertTrue(((AssignStmt) stmt).getVar().getDecl() instanceof  ParamDecl);
        ParamDecl var =  (ParamDecl) ((AssignStmt) stmt).getVar().getDecl();
        assertTrue(var.getAccess() instanceof InterfaceTypeUse);
        assertEquals("M_A.I", ((InterfaceTypeUse) var.getAccess()).getName());
        assertTrue(((AssignStmt)class4.getMethod(0).getBlock().getStmt(0) ).getValue() instanceof NewExp);
        NewExp ne = (NewExp)((AssignStmt)class4.getMethod(0).getBlock().getStmt(0) ).getValue();
        ClassDecl cA = (ClassDecl) findDecl(model, "M_A", "C");
        assertEquals("M_A.C" ,ne.getClassName());
        assertEquals(cA, ((UnionType) ne.getType()).getOriginatingClass());

        stmt = class4.getMethod(0).getBlock().getStmt(1);
        assertTrue( stmt instanceof AssignStmt);
        assertTrue(((AssignStmt) stmt).getVar().getDecl() instanceof  ParamDecl);
        var =  (ParamDecl) ((AssignStmt) stmt).getVar().getDecl();
        assertTrue(var.getAccess() instanceof InterfaceTypeUse);
        assertEquals("M_B.I", ((InterfaceTypeUse) var.getAccess()).getName());
        assertTrue(((AssignStmt)stmt ).getValue() instanceof NewExp);
        ne = (NewExp)((AssignStmt) stmt).getValue();
        ClassDecl cB = (ClassDecl) findDecl(model, "M_B", "C");
        assertEquals("M_B.C" ,ne.getClassName());
        assertEquals(cB, ((UnionType) ne.getType()).getOriginatingClass());

        stmt = class4.getMethod(0).getBlock().getStmt(2);
        assertTrue( stmt instanceof AssignStmt);
        assertTrue(((AssignStmt) stmt).getVar().getDecl() instanceof  ParamDecl);
        var =  (ParamDecl) ((AssignStmt) stmt).getVar().getDecl();
        assertTrue(var.getAccess() instanceof InterfaceTypeUse);
        assertEquals("I2", ((InterfaceTypeUse) var.getAccess()).getName());
        assertTrue(((AssignStmt)stmt ).getValue() instanceof NewExp);
        ne = (NewExp)((AssignStmt) stmt).getValue();
        ClassDecl c2 = (ClassDecl) findDecl(model, "M_A", "C2");
        assertEquals("M_A.C2" ,ne.getClassName());
        assertEquals(c2, ((UnionType) ne.getType()).getOriginatingClass());

        stmt = class4.getMethod(0).getBlock().getStmt(3);
        assertTrue( stmt instanceof AssignStmt);
        assertTrue(((AssignStmt) stmt).getVar().getDecl() instanceof  ParamDecl);
        var =  (ParamDecl) ((AssignStmt) stmt).getVar().getDecl();
        assertTrue(var.getAccess() instanceof InterfaceTypeUse);
        assertEquals("I2", ((InterfaceTypeUse) var.getAccess()).getName());
        assertTrue(((AssignStmt)stmt ).getValue() instanceof NewExp);
        ne = (NewExp)((AssignStmt) stmt).getValue();
        ClassDecl c3 = (ClassDecl) findDecl(model, "M", "C3");
        assertEquals("C3" ,ne.getClassName());
        assertEquals(c3, ((UnionType) ne.getType()).getOriginatingClass());
    }
    @Test
    public void methodReturnType() throws WrongProgramArgumentException {
        Model model = assertParse(
            vm
                + "module TestMain;"
                + "import * from M;"
                + "class C4 () { "
                + "  I setV1(I p1) {"
                + "    p1 = new C () with {A};"
                + "    return p1;"
                + "  }"
                + "}"
                
        );
        model.flattenforLocalProducts();
        SemanticConditionList errors = model.getTypeErrors();
        assertEquals(0, model.getTypeErrors().getErrorCount());
        ClassDecl class4 = (ClassDecl) findDecl(model, "TestMain", "C4");
        assertNotNull(class4);
        MethodSig methodSig = class4.getMethod(0).getMethodSig();

        assertTrue(methodSig.getParam(0).getAccess() instanceof InterfaceTypeUse);
        assertTrue(! (methodSig.getParam(0).getAccess() instanceof VariableInterfaceTypeUse));
        assertEquals("M_A.I", ((InterfaceTypeUse)methodSig.getParam(0).getAccess()).getName());

        assertTrue(methodSig.getReturnType() instanceof InterfaceTypeUse);
        assertTrue(! (methodSig.getReturnType() instanceof VariableInterfaceTypeUse));
        assertEquals("M_A.I", ((InterfaceTypeUse)methodSig.getReturnType()).getName());

    }

    @Test
    public void methodReturnType2() throws WrongProgramArgumentException {
        Model model = assertParse(
            vm
                + "module TestMain;"
                + "import * from M;"
                + "class C4 () { "
                + "  I setV1(I with P1 p1) {"
                + "    p1 = new C ();"
                + "    return p1;"
                + "  }"
                + "}"
                
        );
        model.flattenforLocalProducts();
        assertEquals(0, model.getTypeErrors().getErrorCount());
        ClassDecl class4 = (ClassDecl) findDecl(model, "TestMain", "C4");
        assertNotNull(class4);
        MethodSig methodSig = class4.getMethod(0).getMethodSig();
        assertTrue(methodSig.getParam(0).getAccess() instanceof InterfaceTypeUse);
        assertTrue(! (methodSig.getParam(0).getAccess() instanceof VariableInterfaceTypeUse));
        assertEquals("M_A.I", ((InterfaceTypeUse)methodSig.getParam(0).getAccess()).getName());

        assertTrue(methodSig.getReturnType() instanceof InterfaceTypeUse);
        assertTrue(! (methodSig.getReturnType() instanceof VariableInterfaceTypeUse));
        assertEquals("M_A.I", ((InterfaceTypeUse)methodSig.getReturnType()).getName());

        Stmt stmt = class4.getMethod(0).getBlock().getStmt(0);
        assertTrue( stmt instanceof AssignStmt);
        assertTrue(((AssignStmt) stmt ).getValue() instanceof NewExp);
        NewExp ne = (NewExp)((AssignStmt)stmt).getValue();
        ClassDecl cA = (ClassDecl) findDecl(model, "M_A", "C");
        assertEquals("M_A.C" ,ne.getClassName());
        assertEquals(cA, ((UnionType) ne.getType()).getOriginatingClass());
    }

    @Test
    public void methodReturnType3() throws WrongProgramArgumentException {
        Model model = assertParse(
            vm
                + "module TestMain;"
                + "import * from M;"
                + "class C4 () { "
                + "  I with {A} setV1(I p1) {"
                + "    p1 = new C ();"
                + "    return p1;"
                + "  }"
                + "  I with {B} set2(I p1) {"
                + "    p1 = new C ();"
                + "    return p1;"
                + "  }"
                + "}"
                
        );
        model.flattenforLocalProducts();
        ClassDecl class4 = (ClassDecl) findDecl(model, "TestMain", "C4");
        MethodSig methodSig = class4.getMethod(0).getMethodSig();
        assertTrue(methodSig.getParam(0).getAccess() instanceof InterfaceTypeUse);
        assertTrue(! (methodSig.getParam(0).getAccess() instanceof VariableInterfaceTypeUse));
        assertEquals("M_A.I", ((InterfaceTypeUse)methodSig.getParam(0).getAccess()).getName());

        assertTrue(methodSig.getReturnType() instanceof InterfaceTypeUse);
        assertTrue(! (methodSig.getReturnType() instanceof VariableInterfaceTypeUse));
        assertEquals("M_A.I", ((InterfaceTypeUse)methodSig.getReturnType()).getName());

        Stmt stmt = class4.getMethod(0).getBlock().getStmt(0);
        assertTrue( stmt instanceof AssignStmt);
        assertTrue(((AssignStmt) stmt ).getValue() instanceof NewExp);
        NewExp ne = (NewExp)((AssignStmt)stmt).getValue();
        assertNotEquals("M_A.C" ,ne.getClassName());
        assertEquals(2, model.getTypeErrors().getErrorCount());

    }

    @Test
    public void methodReturnType4() throws WrongProgramArgumentException {
        Model model = assertParse(
            vm
                + "module TestMain;"
                + "import * from M;"
                + "class C4 () { "
                + "  I setV1(I p1) {"
                + "    I with P1 p2 = p1;"
                + "    return p2;"
                + "  }"
                + "  I with {B} set2(I p1) {"
                + "    p1 = new C () with {B};"
                + "    return p1;"
                + "  }"
                + "}"
                
        );
        model.flattenforLocalProducts();
        SemanticConditionList errors = model.getTypeErrors();
        assertEquals(0, errors.getErrorCount());

    }
    @Test
    public void invalidTypes() throws WrongProgramArgumentException {
        Model model = assertParse(
            vm
                + "module TestMain;"
                + "import * from M;"
                + "class C4 (I with P1 p1) { "
                + "  I with {B} v1 = p1;"
                + "}"
                
        );
        model.flattenforLocalProducts();
        SemanticConditionList errors = model.getTypeErrors();
        assertEquals(1, errors.getErrorCount());
        assertEquals(ErrorMessage.CANNOT_ASSIGN, errors.getFirstError().msg);
    }
}
