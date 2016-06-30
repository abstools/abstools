/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.typesystem;

import static org.junit.Assert.*;

import java.io.*;

import org.junit.Test;

import abs.common.FileUtils;
import abs.frontend.FrontendTest;
import abs.frontend.analyser.SemanticConditionList;
import abs.frontend.ast.ClassDecl;
import abs.frontend.ast.Model;
import abs.frontend.ast.VarDeclStmt;
import abs.frontend.typechecker.locationtypes.LocationType;
import abs.frontend.typechecker.locationtypes.LocationTypeCheckerException;
import abs.frontend.typechecker.locationtypes.LocationTypeExtension;
import abs.frontend.typechecker.locationtypes.infer.InferMain;
import abs.frontend.typechecker.locationtypes.infer.LocationTypeInferrerExtension;
import abs.frontend.typechecker.locationtypes.infer.LocationTypeInferrerExtension.LocationTypingPrecision;
import static abs.ABSTest.Config.*;

public class LocationTypeTests extends FrontendTest {
    
    @Test
    public void fieldDecl() {
        Model m = assertParse("interface I { } class C { [Far] I i; }",WITH_STD_LIB);
        ClassDecl decl = getFirstClassDecl(m);
        LocationType ft = LocationTypeExtension.getLocationTypeFromAnnotations(decl.getField(0).getType());
        assertEquals(LocationType.FAR,ft);
    }

    @Test
    public void varDecl() {
        Model m = assertParse("interface I { } { [Somewhere] I i; [Near] I jloc; i = jloc; }",WITH_STD_LIB);
        m.typeCheck();
        assertEquals(LocationType.NEAR,LocationTypeExtension.getLocationTypeFromAnnotations(getTypeOfFirstAssignment(m)));
    }
    private static String INT = "interface I { [Near] I m(); [Far] I n([Near] I i); Unit farM([Far] I i);}" +
    		" class C([Somewhere] I f) implements I { " +
    		"    [Far] I farField; " +
    		"    [Near] I nearField; " +
    		"    [Near] I m() { [Near] I i; i = this; return nearField; }  " +
    		"    [Far] I n([Near] I i) { return farField; }" +
    		"    Unit farM([Far] I i) { }}" +
    		" interface J { } class E implements J { }";

    
    @Test
    public void syncCall() {
        assertLocationTypeOk("{ [Near] I i; i = i.m(); }");
    }

    @Test
    public void asyncCall() {
        assertLocationTypeOk("{ [Far] I i; Fut<[Far] I> f; f = i!m(); }");
    }
    
    @Test
    public void syncCallOnThis() {
        assertLocationTypeOk("class D { Unit m() { this.m(); } }");
    }
    
    @Test
    public void nullLit() {
        assertLocationTypeOk("{ [Near] I i; i = null; [Far] I j; j = null; }");
    }
    
    @Test
    public void syncCallParam() {
        assertLocationTypeOk("{ [Near] I i; [Far] I j; j = i.n(i); }");
    }

    @Test
    public void newCog() {
        assertLocationTypeOk("{ [Far] I i; i = new C(i); }");
    }

    @Test
    public void newObject() {
        assertLocationTypeOk("{ [Near] J i; i = new local E(); }");
        assertLocationTypeOk("{ [Somewhere] J i; i = new local E(); }");
    }

    @Test
    public void typeMaybe() {
        assertLocationTypeOk("{ [Near] I i; Maybe<[Near] I> m = Just(i); }");
    }
    
    @Test
    public void syncCallOnMaybeThis() {
        String s = "def X id<X>(X x) = x; interface K { Unit m(Maybe<[Near] K> p); } " +
          "class D implements K { [Near] K getThis() { return this; } Unit m(Maybe<[Near] K> p)";
        assertLocationTypeOk(s+ "{ this.m(Just(this)); } }");
        assertLocationTypeOk(s+ "{ [Near] K k; k = this; this.m(Just(k)); } }");
        assertLocationTypeOk(s+ "{ [Near] K k; this.m(case Just(k) { Just(x) => Just(x); }); } }");
        assertLocationTypeOk(s+ "{ [Near] K k; this.m(Just(id(k))); } }");
    }

    @Test
    public void typeParamInference() {
        assertLocationTypeOk("{ [Near] I i; Maybe<Maybe<Bool>> m = Nothing; }");
    }
    
    @Test
    public void defaultTyping() {
        assertLocationTypeOk("{ I i; [Far] I f; i = new local C(f); }");
    }
    
    @Test
    public void simpleAssignInfer() {
        assertInferOk("interface I {} { I i; [Far] I i2; i = i2; }", LocationType.FAR);
    }

    @Test
    public void futureTyping() {
        assertLocationTypeOk("{ I i; [Far] I f; Fut<I> fut; i = new local C(f); fut = i!m(); }");
    }
    
    
    @Test
    public void syncCallInfer() {
        assertInferOk("interface I { Unit m(); } { I i; i.m(); }", LocationType.NEAR);
    }

    @Test
    public void asyncCallInfer() {
        assertInferOk("interface I { [Near] I m(); } { I j; [Far] I i; Fut<I> f; f = i!m(); j = f.get; }", LocationType.FAR);
    }
    
    @Test
    public void testListConstruct() {
        assertLocationTypeErrorOnly("interface K {} { List<[Near] K> res = Nil; [Far] K j; res = Cons(j,Nil); }");
    }

    @Test
    public void patternMatch() {
        assertLocationTypeErrorOnly("interface K {} { Maybe<[Near] K> m = Nothing; [Far] K k = case m { Nothing => null; Just(x) => x; };  }");
    }

    @Test
    public void caseExp() {
        assertLocationTypeError("interface K {} { Maybe<[Far] K> m = Nothing; [Near] K knear;" +
        		"[Far] K k = case m { Nothing => knear; Just(x) => x;  };  }");
    }
    
    @Test
    public void function() {
        assertLocationTypeErrorOnly("interface K {} def [Near] K f([Somewhere] K k) = k;");
    }

    @Test
    public void fnapp() {
        assertLocationTypeErrorOnly("interface K {} def Unit f([Near] K k) = Unit; { [Far] K k; Unit u = f(k);}");
    }

    @Test
    public void dataExp() {
        assertLocationTypeErrorOnly("interface K {} data Foo = Bar([Far] K); { [Near] K k; Foo f = Bar(k);}");
    }
    
    /*
    @Test
    public void callParamInfer() {
        Model m = assertInferOk("interface I { Unit m(I i); } class C implements I { Unit m(I i) { I j; j = new local C(); i.m(j); } } { }");
        ClassDecl cd = (ClassDecl) m.getCompilationUnit(1).getModuleDecl(0).getDecl(1);
        Type t = cd.getMethod(0).getMethodSig().getParam(0).getType();
        LocationType lt = m.getLocationTypeInferenceResult().get(LocationTypeInferrerExtension.getLV(t));
        assertEquals(LocationType.NEAR, lt);
    }*/
    
    
    @Test
    public void newCOGInfer() {
        assertInferOk("interface I { } class C implements I {} { I i; i = new C(); }", LocationType.FAR);
    }

    @Test
    public void newCOGInferAnn() {
        assertInferOk("interface I { } class C implements I {} { [Infer] I i; i = new C(); }", LocationType.FAR);
    }
    
    @Test
    public void newObjectInfer() {
        assertInferOk("interface I { } class C implements I {} { I i; i = new local C(); }", LocationType.NEAR);
    }

    @Test
    public void newObjectInfer2() {
        assertInferOk("interface I { I getI(); } class C implements I { I i; { i = new local C(); } I getI() { return i; } } " +
        		"{ I i; I j; j = new local C(); i = j.getI(); }", LocationType.NEAR);
    }
    
    @Test
    public void newObjectInfer3() {
        assertInferOk("interface I { } class C implements I {} { I i; i = new local C(); i = new C(); }", LocationType.SOMEWHERE);
    }
    
    @Test
    public void annotatedlocaVarInfer() {
        assertInferOk("interface I { } class C implements I {} { I i; [Near] I j; i = j; }", LocationType.NEAR);
    }
    
    @Test
    public void overrideOK() {
        assertInferOk("interface I { [Somewhere] I m([Far] I i); } class C implements I { [Near] I m([Somewhere] I i) { return null; } } { }");
    }
    
    @Test
    public void overrideminimal() {
        assertInferOk("interface I { I m([Far] I i); } class C implements I { [Near] I m([Somewhere] I i) { return null; } } { I i; [Near] I k; Fut<I> j; j = k!m(null); i = j.get; }", LocationType.NEAR);
    }
    
    @Test
    public void callNullParam() {
        assertTypeOkOnly("interface I2 { Unit m([Near] I2 i); } { [Far] I2 i; i!m(null); }");
    }
    
    @Test
    public void callNullParam2() {
        assertLocationTypeOk("interface I2 { Unit m([Near] I2 i); } { [Somewhere] I2 i; i!m(null); }");
    }

    @Test
    public void callReturn() {
        assertInferOk("interface I2 { [Near] I2 m(); }  { I2 i2; [Far] I2 i; Fut<I2> f; f = i!m(); i2 = f.get; }", LocationType.FAR);
    }

    @Test
    public void callReturn2() {
        assertInferOk("module M.S1; export *; interface I { [Near] I m(); } class C implements I { [Near] I m() { return null; } } " +
                      "module M.S2; import * from M.S1; { I i; i = new C(); i!m(); }", LocationType.FAR);
    }
    
    @Test
    public void typeSyn() {
        assertInferOk("interface I {} type I2 = [Somewhere] I; { I2 i; i = null; }", LocationType.SOMEWHERE);
    }
    
    @Test
    public void typeImprovedInfer() {
        assertInferOk("interface I { Unit m([Far] I i); } class C implements I { Unit m([Far] I i) { } } { I i1; I i2; i1 = new C(); i2 = new C(); i1!m(i2); }");
    }
    
    @Test
    public void fieldTypeImprovedInfer() {
        assertInferOk("interface I { Unit m([Far] I i); } " +
        	"class C implements I { " +
        	"    I i1; I i2; " +
        	"    Unit m([Far] I i) { i1 = new C(); i2 = new C(); i1!m(i2); } } { }");
    }
    
    @Test
    public void writeBackTest() throws Exception {
        String s = writeBackSolutions("module M; interface I { Unit m([Far] I i); } class C implements I { Unit m([Far] I i) { } } { I i1; I i2; i1 = new C(); i2 = new C(); i1!m(i2); }");
        //System.out.println(s);
        // TODO: Do something later (2010+)
        assertTypeOK(s);
        assertEquals("module M; interface I { Unit m([Far] I i); } class C implements I { Unit m([Far] I i) { } } { [Far] I i1; [Far] I i2; i1 = new C(); i2 = new C(); i1!m(i2); }",s);
    }
    
    // negative tests:

    @Test
    public void typeSynFail() {
        assertInferFails("interface I {} type I2 = [Somewhere] I; { [Far] I2 i; i = null; }");
    }
    
   
    @Test
    public void typeMaybeError() {
        assertLocationTypeErrorOnly("interface I { } { [Far] I i; Maybe<[Near] I> m = Just(i); }");
    }

    @Test
    public void typeListError() {
        assertLocationTypeErrorOnly("interface I {} { List<[Far] I> list = Nil; [Near] I i; list = Cons(i,list); }");
    }
    
    @Test
    public void callWrongParam() {
        assertLocationTypeErrorOnly("interface I { Unit m([Near] I i); } { [Far] I i; i!m(i); }");
    }
    
    @Test
    public void assignWrong() {
        assertLocationTypeError("{ [Far] I i; [Near] I j; i = j; }");
    }

    @Test
    public void illegalSyncCall() {
        assertLocationTypeError("{ [Far] I i; i.m(); }");
    }

    @Test
    public void illegalAsyncCall() {
        assertLocationTypeError("{ [Far] I i; i!farM(i); }");
    }
    
    @Test
    public void syncCallWrongParam() {
        assertLocationTypeError("{ [Near] I i; [Far] I j; j = i.n(j); }");
    }
    
    @Test(expected=LocationTypeCheckerException.class)
    public void multipleError() {
        Model m = assertParse("interface I { } class C { [Far] [Near] I i; }",WITH_STD_LIB);
        ClassDecl decl = getFirstClassDecl(m);
        LocationTypeExtension.getLocationTypeFromAnnotations(decl.getField(0).getType());
    }
    
    @Test
    public void overrideReturnFailed() {
        assertInferFails("interface I { [Near] I m(I i); } class C implements I { [Somewhere] I m(I i) { return null; } } { }");
    }
    
    @Test
    public void overrideParameterFailed() {
        assertInferFails("interface I { I m([Somewhere] I i); } class C implements I { I m([Near] I i) { return null; } } { }");
    }
    
    @Test
    public void testInferenceRetypeChecking() {
        String code = "interface I { Unit m(); } { [Far] I i; I j; i = j; j.m(); }";
        Model m = assertParseOkStdLib(code);
        LocationTypeExtension te = new LocationTypeExtension(m);
        m.registerTypeSystemExtension(te);
        SemanticConditionList e = m.typeCheck();
        m.typeCheck(new SemanticConditionList());
    }
    
    @Test
    public void testAwaitFail() {
        LocationType lt = LocationType.INFER;
        Model m = assertParseOkStdLib("interface T { Unit foo(); } class C { T t = null; Unit bar() { await t!foo(); }}");
        assertFalse(m.hasErrors()); // This line is essential to trigger the NPE!
        LocationTypeInferrerExtension ltie = new LocationTypeInferrerExtension(m);
        ltie.setDefaultType(lt);
        ltie.setLocationTypingPrecision(LocationTypingPrecision.CLASS_LOCAL_FAR);
        m.registerTypeSystemExtension(ltie);
        m.getErrors();
        SemanticConditionList e = m.typeCheck();
    }
    
    @Test
    public void testAwaitFailRewriteOff() {
        LocationType lt = LocationType.INFER;
        Model.doAACrewrite = false;
        Model m = assertParseOkStdLib("interface T { Unit foo(); } class C { T t = null; Unit bar() { await t!foo(); }}");
        assertFalse(m.hasErrors()); // This line is essential to trigger the NPE!
        LocationTypeInferrerExtension ltie = new LocationTypeInferrerExtension(m);
        ltie.setDefaultType(lt);
        ltie.setLocationTypingPrecision(LocationTypingPrecision.CLASS_LOCAL_FAR);
        m.registerTypeSystemExtension(ltie);
        m.getErrors();
        SemanticConditionList e = m.typeCheck();
        Model.doAACrewrite = true;
    }

    private void assertLocationTypeError(String code) {
        assertLocationTypeErrorOnly(INT+code);
    }
    
    private void assertLocationTypeErrorOnly(String code) {
        Model m = assertParse(code,WITH_STD_LIB);
        LocationTypeExtension te = new LocationTypeExtension(m);
        m.registerTypeSystemExtension(te);
        SemanticConditionList e = m.typeCheck();
        assertFalse("No type error occurred", !e.containsErrors());
        assertInferFails(code);
    }
    
    private void assertLocationTypeOk(String code) {
        assertTypeOkOnly(INT+code);
    }

    private void assertTypeOkOnly(String code) {
        Model m = assertParse(code,WITH_STD_LIB);
        LocationTypeExtension te = new LocationTypeExtension(m);
        m.registerTypeSystemExtension(te);
        m.getErrors();
        SemanticConditionList e = m.typeCheck();
        assertTrue(!e.containsErrors() ? "" : "Found error "+e.getFirstError().getMessage(),!e.containsErrors());
        assertInferOk(code);
    }
    
    private Model assertInfer(String code, LocationType expected, boolean fails) {
        Model m = assertParse(code,WITH_STD_LIB);
        //m.setLocationTypingEnabled(true);
        LocationTypeInferrerExtension ltie = new LocationTypeInferrerExtension(m);
        m.registerTypeSystemExtension(ltie);
        SemanticConditionList e = m.typeCheck();
        //System.out.println(ltie.getConstraints());
        assertEquals(!e.containsErrors() ? "" : "Found error: "+e.getFirstError().getMessage(), fails, e.containsErrors()); 
        
        //assertTrue("Inference failed", generated != null);
        //assertEquals(fails, generated == null);
        if (expected != null) {
            VarDeclStmt vds = ((VarDeclStmt)m.getMainBlock().getStmt(0));
            LocationType t = ltie.getResults().get(LocationTypeInferrerExtension.getLV(vds.getVarDecl().getType()));
            assertTrue(t.toString(), expected == LocationType.FAR ? t == LocationType.FAR || t.isParametricFar() : expected == t);
        }
        return m;
    }
    
    private String writeBackSolutions(String code) throws Exception {
        File f = File.createTempFile("test", ".abs");
        f.deleteOnExit();
        FileWriter fw = new FileWriter(f);
        fw.write(code);
        fw.close();
        Model m = assertParseFileOk(f.getAbsolutePath(), Config.WITH_STD_LIB);
        LocationTypeInferrerExtension ltie = new LocationTypeInferrerExtension(m);
        m.registerTypeSystemExtension(ltie);
        SemanticConditionList e = m.typeCheck();
        assertEquals(!e.containsErrors() ? "" : "Found error: "+e.getFirstError().getMessage(), false, e.containsErrors());
        new InferMain().writeInferenceResultsBack(ltie.getResults());
        String res = FileUtils.fileToStringBuilder(f).toString();
        f.delete();
        return res;
    }
    
    private Model assertInferOk(String string, LocationType expected) {
        return assertInfer(string, expected, false);
    }
    
    private Model assertInferOk(String string) {
        return assertInfer(string, null, false);       
    }
    
    private void assertInferFails(String string) {
        assertInfer(string, null, true);
    }
}
