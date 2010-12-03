package abs.frontend.typesystem;

import static org.junit.Assert.assertEquals;

import java.util.List;

import org.junit.Test;

import abs.frontend.FrontendTest;
import abs.frontend.ast.ClassDecl;
import abs.frontend.ast.Model;
import abs.frontend.typechecker.Type;
import abs.frontend.typechecker.TypeAnnotation;

public class AnnotationTests extends FrontendTest {

    static final String TEST_ANN = "[TypeAnnotation] data Loc = Far | Near; interface I { [Far] I farM(); [Near] I localM(); } ";
    
    @Test
    public void testVarDecl() {
        assertFirstAssignmentIsLocType("{ [Far] I i = i; }");
    }

    @Test
    public void testMethodCall() {
        assertFirstAssignmentIsLocType("{ I i = i.farM(); }");
    }

    @Test
    public void testMethodParam() {
        Model m = assertParseOkAnn("class C { Unit m([Far] I i) { } }");
        ClassDecl decl = getFirstClassDecl(m);
        assertHasLocAnnotation(decl.getMethod(0).getMethodSig().getParam(0).getType());
    }

    @Test
    public void testFieldDecl() {
        Model m = assertParseOkAnn("class C { [Far] I i; }");
        ClassDecl decl = getFirstClassDecl(m);
        assertHasLocAnnotation(decl.getField(0).getType());
    }

    @Test
    public void testClassParam() {
        Model m = assertParseOkAnn("class C([Far] I i) { }");
        ClassDecl decl = getFirstClassDecl(m);
        assertHasLocAnnotation(decl.getParam(0).getType());
    }
    
    private void assertFirstAssignmentIsLocType(String exampleCode) {
        Model m = assertParseOkAnn(exampleCode);
        Type t = getTypeOfFirstAssignment(m);

        assertHasLocAnnotation(t);
    }

    private Model assertParseOkAnn(String exampleCode) {
        return assertParseOk(TEST_ANN+exampleCode,true);
    }

    private void assertHasLocAnnotation(Type t) {
        List<TypeAnnotation> anns = t.getTypeAnnotations();
        TypeAnnotation a = anns.get(0);
        assertEquals("Loc",a.getType().getSimpleName());
    }
    

}
