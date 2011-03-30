/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.typesystem;

import static org.junit.Assert.assertEquals;

import java.util.List;

import org.junit.Test;

import abs.frontend.FrontendTest;
import abs.frontend.ast.ClassDecl;
import abs.frontend.ast.DataConstructorExp;
import abs.frontend.ast.Model;
import abs.frontend.ast.VarDeclStmt;
import abs.frontend.typechecker.Type;
import abs.frontend.typechecker.TypeAnnotation;
import static abs.ABSTest.Config.*;

public class AnnotationTests extends FrontendTest {

    static final String TEST_ANN = "interface I { [Far] I farM(); [Near] I localM(); } ";
    
    @Test
    public void testVarDecl() {
        Model m = assertParseOkAnn("{ [Near] I i; }");
        VarDeclStmt v = ((VarDeclStmt)m.getMainBlock().getStmt(0));
        assertHasLocAnnotation(v.getVarDecl().getType(),"Near");
    }
    
    @Test
    public void testMethodParam() {
        Model m = assertParseOkAnn("class C { Unit m([Far] I i) { } }");
        ClassDecl decl = getFirstClassDecl(m);
        assertHasLocAnnotation(decl.getMethod(0).getMethodSig().getParam(0).getType(),"Far");
    }

    @Test
    public void testFieldDecl() {
        Model m = assertParseOkAnn("class C { [Far] I i; }");
        ClassDecl decl = getFirstClassDecl(m);
        assertHasLocAnnotation(decl.getField(0).getType(),"Far");
    }

    @Test
    public void testClassParam() {
        Model m = assertParseOkAnn("class C([Far] I i) { }");
        ClassDecl decl = getFirstClassDecl(m);
        assertHasLocAnnotation(decl.getParam(0).getType(),"Far");
    }
    
    private Model assertParseOkAnn(String exampleCode) {
        return assertParse(TEST_ANN+exampleCode,WITH_STD_LIB);
    }

    private void assertHasLocAnnotation(Type t, String s) {
        List<TypeAnnotation> anns = t.getTypeAnnotations();
        TypeAnnotation a = anns.get(0);
        assertEquals("LocationType",a.getType().getSimpleName());
        assertEquals(s, ((DataConstructorExp)a.getValue()).getDecl().getName());
    }
    

}
