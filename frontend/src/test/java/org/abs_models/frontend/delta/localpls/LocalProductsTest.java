package org.abs_models.frontend.delta.localpls;

import org.abs_models.common.WrongProgramArgumentException;
import org.abs_models.frontend.analyser.ErrorMessage;
import org.abs_models.frontend.analyser.SemanticConditionList;
import org.abs_models.frontend.ast.*;
import org.abs_models.frontend.delta.DeltaTest;
import org.junit.Assert;
import org.junit.Test;

import static org.junit.Assert.*;
import static org.junit.Assert.assertFalse;

public class LocalProductsTest extends DeltaTest {

    String vmAllBase = "module M;"
        + "export *;"
        + "base interface I{}"
        + "base relative class C implements I {}"
        + "features A;"
        + "delta D1;"
        + "modifies interface I {"
        + " adds Unit me1(); }"
        + "modifies class C {"
        + " adds Unit me1(){"
        + "    println(\"S\");"
        + "} }"
        + "delta D1 when A;";

    String vm = "module M;"
        + "export *;"
        + "interface I{}"
        + "base class C implements I {}"
        + "features A;"
        + "delta D1;"
        + "modifies class C {"
        + " adds Unit me1(){"
        + "    println(\"S\");"
        + "} }"
        + "delta D1 when A;";
    @Test
    public void productITUinVariable() throws WrongProgramArgumentException {
        Model model = assertParse(
            vmAllBase
                + "module TestMain;"
                + "import * from M;"
                + " { I with LocalProduct m; }"
                +"product LocalProduct = {A};"
        );
        model.flattenforLocalProducts();
        assertEquals(0, model.getTypeErrors().getErrorCount());
        assertNotNull(model.lookupModule("M_A"));
        ModuleDecl testModule = model.lookupModule("TestMain");
        assertEquals(1, testModule.getBlock().getNumStmt());
        Stmt s = testModule.getBlock().getStmt(0);
        Assert.assertTrue(s instanceof VarDeclStmt);
        assertEquals("M_A.I", ((InterfaceTypeUse) ((VarDeclStmt) s).getVarDecl().getAccess()).getName());
        assertFalse(((VarDeclStmt) s).getVarDecl().getAccess() instanceof VariableInterfaceTypeUse);
    }
    @Test
    public void productITUinVariable2() throws WrongProgramArgumentException {
        Model model = assertParse(
            vmAllBase
                + "module TestMain;"
                + "import * from M;"
                + " { I with {A} m; }"
        );
        model.flattenforLocalProducts();
        assertEquals(0, model.getTypeErrors().getErrorCount());
        assertNotNull(model.lookupModule("M_A"));
        ModuleDecl testModule = model.lookupModule("TestMain");
        assertEquals(1, testModule.getBlock().getNumStmt());
        Stmt s = testModule.getBlock().getStmt(0);
        Assert.assertTrue(s instanceof VarDeclStmt);
        assertEquals("M_A.I", ((VarDeclStmt) s).getVarDecl().getAccess().getType().getQualifiedName());
        assertFalse(((VarDeclStmt) s).getVarDecl().getAccess() instanceof VariableInterfaceTypeUse);
    }
    @Test
    public void productITUinExtends() throws WrongProgramArgumentException {
        Model model = assertParse(
                vmAllBase
                + "module TestMain;"
                + "import * from M;"
                + "interface ITest extends I with LocalProduct{ }"
                +"product LocalProduct = {A};"
        );
        model.flattenforLocalProducts();
        assertEquals(0, model.getTypeErrors().getErrorCount());
        assertNotNull(model.lookupModule("M_A"));
        InterfaceDecl interface1 = (InterfaceDecl) findDecl(model, "TestMain", "ITest");
        assertNotNull(interface1);
        assertEquals(1,interface1.getNumExtendedInterfaceUse());
        assertEquals("M_A.I", interface1.getExtendedInterfaceUse(0).getName());
        assertFalse(interface1.getExtendedInterfaceUse(0) instanceof VariableInterfaceTypeUse);

    }
    @Test
    public void productITUinExtends2() throws WrongProgramArgumentException {
        Model model = assertParse(
            vmAllBase
                + "module TestMain;"
                + "import * from M;"
                + "interface ITest extends I with {A} { }"
                +"product LocalProduct = {A};"
        );
        model.flattenforLocalProducts();
        assertEquals(0, model.getTypeErrors().getErrorCount());
        assertNotNull(model.lookupModule("M_A"));
        InterfaceDecl interface1 = (InterfaceDecl) findDecl(model, "TestMain", "ITest");
        assertNotNull(interface1);
        assertEquals(1,interface1.getNumExtendedInterfaceUse());
        assertEquals("M_A.I", interface1.getExtendedInterfaceUse(0).getName());
        assertFalse(interface1.getExtendedInterfaceUse(0) instanceof VariableInterfaceTypeUse);

    }
    @Test
    public void productITUinImplements() throws WrongProgramArgumentException{
        Model model = assertParse(
            vmAllBase
                + "module TestMain;"
                + "import * from M;"
                + "class Test () implements I with LocalProduct{ }"
                + "product LocalProduct = {A};"
        );
        model.flattenforLocalProducts();

        assertNotNull(model.lookupModule("M_A"));
        ClassDecl classDecl = (ClassDecl) findDecl(model, "TestMain", "Test");
        assertEquals(1, classDecl.getNumImplementedInterfaceUse());
        assertEquals("M_A.I", classDecl.getImplementedInterfaceUse(0).getType().getQualifiedName());
        assertFalse(classDecl.getImplementedInterfaceUse(0) instanceof VariableInterfaceTypeUse);

        assertEquals(1, model.getTypeErrors().getErrorCount());
        assertEquals(ErrorMessage.METHOD_NOT_IMPLEMENTED, model.getTypeErrors().getFirstError().msg);

    }
    @Test
    public void productITUinImplements2() throws WrongProgramArgumentException{
        Model model = assertParse(
            vmAllBase
                + "module TestMain;"
                + "import * from M;"
                + "class Test () implements I with {A}{ }"
        );
        model.flattenforLocalProducts();
        assertNotNull(model.lookupModule("M_A"));
        ClassDecl classDecl = (ClassDecl) findDecl(model, "TestMain", "Test");
        assertEquals(1, classDecl.getNumImplementedInterfaceUse());
        assertEquals("M_A.I", classDecl.getImplementedInterfaceUse(0).getName());
        assertFalse(classDecl.getImplementedInterfaceUse(0) instanceof VariableInterfaceTypeUse);
        assertEquals(1, model.getTypeErrors().getErrorCount());
        assertEquals(ErrorMessage.METHOD_NOT_IMPLEMENTED, model.getTypeErrors().getFirstError().msg);

    }
    @Test
    public void productITUinMethod()throws WrongProgramArgumentException {
        Model model = assertParse(
            vmAllBase
                + "module TestMain;"
                + "import * from M;"
                + "class Test {" +
                " I with LocalProduct method1(){"
                + "    return new C();"
                + "}"
                + "}"
                + "product LocalProduct = {A};"
        );
        model.flattenforLocalProducts();
        assertEquals(0, model.getTypeErrors().getErrorCount());
        assertNotNull(model.lookupModule("M_A"));
        ClassDecl classDecl = (ClassDecl) findDecl(model, "TestMain", "Test");
        assertEquals("M_A.I", classDecl.getMethod(0).getType().getQualifiedName());
        assertFalse(classDecl.getMethod(0).getMethodSig().getReturnType() instanceof VariableInterfaceTypeUse);

        Block methodBlock = classDecl.getMethod(0).getBlock();
        Stmt stm = methodBlock.getStmt(methodBlock.getNumStmt() - 1);
        assertTrue(stm instanceof ReturnStmt);
        assertEquals("M_A.C", ((NewExp)((ReturnStmt) stm).getRetExp()).getClassName());
    }
    @Test
    public void productITUinMethod2()throws WrongProgramArgumentException {
        Model model = assertParse(
            vmAllBase
                + "module TestMain;"
                + "import * from M;"
                + "class Test {" +
                " I with {A} method1(){"
                + "    return new C();"
                + "}"
                + "}"
        );
        model.flattenforLocalProducts();
        assertEquals(0, model.getTypeErrors().getErrorCount());
        assertNotNull(model.lookupModule("M_A"));
        ClassDecl classDecl = (ClassDecl) findDecl(model, "TestMain", "Test");
        assertEquals("M_A.I", classDecl.getMethod(0).getType().getQualifiedName());
        assertFalse(classDecl.getMethod(0).getMethodSig().getReturnType() instanceof VariableInterfaceTypeUse);

        Block methodBlock = classDecl.getMethod(0).getBlock();
        Stmt stm = methodBlock.getStmt(methodBlock.getNumStmt() - 1);
        assertTrue(stm instanceof ReturnStmt);
        assertEquals("M_A.C", ((NewExp)((ReturnStmt) stm).getRetExp()).getClassName());
    }
    @Test
    public void productITUasMethodParam() throws WrongProgramArgumentException {
        Model model = assertParse(
            vmAllBase
                + "module TestMain;"
                + "import * from M;"
                + "class Test {" +
                " Unit method1(I with LocalProduct p){"
                + "    println(\"Local\"); }"
                + "}"
                +"product LocalProduct = {A};"
        );
        model.flattenforLocalProducts();
        assertEquals(0, model.getTypeErrors().getErrorCount());
        assertNotNull(model.lookupModule("M_A"));
        ClassDecl classDecl = (ClassDecl) findDecl(model, "TestMain", "Test");

        assertEquals(1,classDecl.getMethod(0).getMethodSig().getNumParam());
        assertEquals("M_A.I",classDecl.getMethod(0).getMethodSig().getParam(0).getType().getQualifiedName());
        assertFalse(classDecl.getMethod(0).getMethodSig().getParam(0).getAccess() instanceof VariableInterfaceTypeUse);
    }
    @Test
    public void productITUasMethodParam2() throws WrongProgramArgumentException {
        Model model = assertParse(
            vmAllBase
                + "module TestMain;"
                + "import * from M;"
                + "class Test {" +
                " Unit method1(I with {A} p){"
                + "    println(\"Local\"); }"
                + "}"
        );
        model.flattenforLocalProducts();
        assertEquals(0, model.getTypeErrors().getErrorCount());
        assertNotNull(model.lookupModule("M_A"));
        ClassDecl classDecl = (ClassDecl) findDecl(model, "TestMain", "Test");

        assertEquals(1,classDecl.getMethod(0).getMethodSig().getNumParam());
        assertEquals("M_A.I", classDecl.getMethod(0).getMethodSig().getParam(0).getType().getQualifiedName());
        assertFalse(classDecl.getMethod(0).getMethodSig().getParam(0).getAccess() instanceof VariableInterfaceTypeUse);
    }

    @Test
    public void productITUasClassParam() throws WrongProgramArgumentException{
        Model model = assertParse(
            vmAllBase
                + "module TestMain;"
                + "import * from M;"
                + "class Test ( I with LocalProduct p){"
                + "}"
                +"product LocalProduct = {A};"
        );
        model.flattenforLocalProducts();
        assertEquals(0, model.getTypeErrors().getErrorCount());
        assertNotNull(model.lookupModule("M_A"));
        ClassDecl classDecl = (ClassDecl) findDecl(model, "TestMain", "Test");
        assertEquals("M_A.I",((InterfaceTypeUse) classDecl.getParam(0).getAccess()).getName());
        assertFalse(classDecl.getParam(0).getAccess() instanceof VariableInterfaceTypeUse);
    }
    @Test
    public void productITUasClassParam2() throws WrongProgramArgumentException{
        Model model = assertParse(
            vmAllBase
                + "module TestMain;"
                + "import * from M;"
                + "class Test ( I with {A} p){"
                + "}"
        );
        model.flattenforLocalProducts();
        assertEquals(0, model.getTypeErrors().getErrorCount());
        assertNotNull(model.lookupModule("M_A"));
        ClassDecl classDecl = (ClassDecl) findDecl(model, "TestMain", "Test");
        assertEquals("M_A.I",classDecl.getParam(0).getType().getQualifiedName());
        assertFalse(classDecl.getParam(0).getAccess() instanceof VariableInterfaceTypeUse);
    }
    @Test
    public void productasITUinNewExp() throws WrongProgramArgumentException {
        Model model = assertParse(
            vmAllBase
                + "module TestMain;"
                + "import * from M;"
                + "class Test (){"
                + " Unit method1() {"
                + " I  p = new C () with LocalProduct;"
                + " }"
                + "}"
                +"product LocalProduct = {A};"
        );
        model.flattenforLocalProducts();
        assertEquals(0, model.getTypeErrors().getErrorCount());
        assertNotNull(model.lookupModule("M_A"));
        ClassDecl classDecl = (ClassDecl) findDecl(model, "TestMain", "Test");

        Stmt stmt = classDecl.getMethod(0).getBlock().getStmt(0);
        assertTrue(stmt instanceof VarDeclStmt);
        assertEquals("M_A.I", ((VarDeclStmt) stmt).getVarDecl().getType().getQualifiedName());
        assertTrue(((VarDeclStmt) stmt).getVarDecl().getInitExp() instanceof NewExp);
        assertEquals("M_A.C", ((NewExp)((VarDeclStmt) stmt).getVarDecl().getInitExp()).getClassName());
    }
    @Test
    public void productasITUinNewExp2() throws WrongProgramArgumentException {
        Model model = assertParse(
            vmAllBase
                + "module TestMain;"
                + "import * from M;"
                + "class Test (){"
                + " Unit method1() {"
                + " I  p = new C () with {A};"
                + " }"
                + "}"
        );
        model.flattenforLocalProducts();
        assertEquals(0, model.getTypeErrors().getErrorCount());
        assertNotNull(model.lookupModule("M_A"));
        ClassDecl classDecl = (ClassDecl) findDecl(model, "TestMain", "Test");

        Stmt stmt = classDecl.getMethod(0).getBlock().getStmt(0);
        assertTrue(stmt instanceof VarDeclStmt);
        assertEquals("M_A.I", ((VarDeclStmt) stmt).getVarDecl().getType().getQualifiedName());
        assertTrue(((VarDeclStmt) stmt).getVarDecl().getInitExp() instanceof NewExp);
        assertEquals("M_A.C", ((NewExp)((VarDeclStmt) stmt).getVarDecl().getInitExp()).getClassName());
    }
    @Test
    public void productITUinNewExp() throws WrongProgramArgumentException{
        Model model = assertParse(
            vmAllBase
                + "module TestMain;"
                + "import * from M;"
                + "class Test (){"
                + " Unit method1() {"
                + " I with LocalProduct p = new C () ;"
                + " }"
                + "}"
                +"product LocalProduct = {A};"
        );
        model.flattenforLocalProducts();
        assertEquals(0, model.getTypeErrors().getErrorCount());
        assertNotNull(model.lookupModule("M_A"));
        ClassDecl classDecl = (ClassDecl) findDecl(model, "TestMain", "Test");

        Stmt stmt = classDecl.getMethod(0).getBlock().getStmt(0);
        assertTrue(stmt instanceof VarDeclStmt);
        assertFalse(((VarDeclStmt) stmt).getVarDecl().getAccess() instanceof VariableInterfaceTypeUse);
        assertEquals("M_A.I", ((VarDeclStmt) stmt).getVarDecl().getType().getQualifiedName());
        assertTrue(((VarDeclStmt) stmt).getVarDecl().getInitExp() instanceof NewExp);
        assertEquals("M_A.C", ((NewExp)((VarDeclStmt) stmt).getVarDecl().getInitExp()).getClassName());
    }

    @Test
    public void productITUinNewExp2() throws WrongProgramArgumentException{
        Model model = assertParse(
            vmAllBase
                + "module TestMain;"
                + "import * from M;"
                + "class Test (){"
                + " Unit method1() {"
                + " I with {A} p = new C () ;"
                + " }"
                + "}"
        );
        model.flattenforLocalProducts();
        assertNotNull(model.lookupModule("M_A"));
        ClassDecl classDecl = (ClassDecl) findDecl(model, "TestMain", "Test");

        Stmt stmt = classDecl.getMethod(0).getBlock().getStmt(0);
        assertTrue(stmt instanceof VarDeclStmt);
        assertFalse(((VarDeclStmt) stmt).getVarDecl().getAccess() instanceof VariableInterfaceTypeUse);
        assertEquals("M_A.I", ((VarDeclStmt) stmt).getVarDecl().getType().getQualifiedName());
        assertTrue(((VarDeclStmt) stmt).getVarDecl().getInitExp() instanceof NewExp);
        assertEquals("M_A.C", ((NewExp)((VarDeclStmt) stmt).getVarDecl().getInitExp()).getClassName());
        assertEquals(0, model.getTypeErrors().getErrorCount());
    }

    @Test
    public void baseClassNoBaseIntf1() throws WrongProgramArgumentException{
        Model model = assertParse(
            vm
                + "module TestMain;"
                + "import * from M;"
                + "class Test () {"
                + " Unit method1() {"
                + " I with {A} p = new C () ;"
                + " }"
                + "}"
        );
        model.flattenforLocalProducts();
        assertEquals(0, model.getTypeErrors().getErrorCount());
        assertNotNull(model.lookupModule("M_A"));
        ClassDecl classDecl = (ClassDecl) findDecl(model, "TestMain", "Test");

        Stmt stmt = classDecl.getMethod(0).getBlock().getStmt(0);
        assertTrue(stmt instanceof VarDeclStmt);
        assertFalse(((VarDeclStmt) stmt).getVarDecl().getAccess() instanceof VariableInterfaceTypeUse);
        assertEquals("I", ((InterfaceTypeUse) ((VarDeclStmt) stmt).getVarDecl().getAccess()).getName());
        assertEquals("M.I", ((VarDeclStmt) stmt).getVarDecl().getType().getQualifiedName());
        assertTrue(((VarDeclStmt) stmt).getVarDecl().getInitExp() instanceof NewExp);
        assertEquals("M_A.C", ((NewExp)((VarDeclStmt) stmt).getVarDecl().getInitExp()).getClassName());
    }
    @Test
    public void baseClassNoBaseIntf2() throws WrongProgramArgumentException{
        Model model = assertParse(
            vm
                + "module TestMain;"
                + "import * from M;"
                + "class Test () {"
                + " Unit method1() {"
                + " I  p = new C () with LocalProduct;"
                + " }"
                + "}"
                + "product LocalProduct = {A};"
        );
        model.flattenforLocalProducts();

        assertEquals(0, model.getTypeErrors().getErrorCount());
        assertNotNull(model.lookupModule("M_A"));
        ClassDecl classDecl = (ClassDecl) findDecl(model, "TestMain", "Test");

        Stmt stmt = classDecl.getMethod(0).getBlock().getStmt(0);
        assertTrue(stmt instanceof VarDeclStmt);

        assertEquals("I", ((InterfaceTypeUse) ((VarDeclStmt) stmt).getVarDecl().getAccess()).getName());
        assertEquals("M.I", ((VarDeclStmt) stmt).getVarDecl().getType().getQualifiedName());
        assertTrue(((VarDeclStmt) stmt).getVarDecl().getInitExp() instanceof NewExp);
        assertEquals("M_A.C", ((NewExp)((VarDeclStmt) stmt).getVarDecl().getInitExp()).getClassName());
    }
    @Test
    public void baseClassNoBaseIntf3() throws WrongProgramArgumentException {
        Model model = assertParse(
            vm
                + "module TestMain;"
                + "import * from M;"
                + "class Test ( M.I with {A} p){"
                + "}"
        );
        model.flattenforLocalProducts();
        assertEquals(0, model.getTypeErrors().getErrorCount());
        assertNotNull(model.lookupModule("M_A"));
        ClassDecl classDecl = (ClassDecl) findDecl(model, "TestMain", "Test");
        assertFalse(classDecl.getParam(0).getAccess() instanceof VariableInterfaceTypeUse);
        assertEquals("M.I", ((InterfaceTypeUse) classDecl.getParam(0).getAccess()).getName());
    }
}
