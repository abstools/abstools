package abs.frontend.typesystem;

import static org.junit.Assert.*;

import org.junit.Test;

import abs.frontend.FrontendTest;
import abs.frontend.analyser.SemanticErrorList;
import abs.frontend.ast.ClassDecl;
import abs.frontend.ast.Model;
import abs.frontend.typechecker.locationtypes.LocationType;
import abs.frontend.typechecker.locationtypes.LocationTypeCheckerException;
import abs.frontend.typechecker.locationtypes.LocationTypeCheckerHelper;

public class LocationTypeTests extends FrontendTest {
    
    @Test
    public void fieldDecl() {
        Model m = assertParseOk("interface I { } class C { [Far] I i; }",true);
        ClassDecl decl = getFirstClassDecl(m);
        LocationType ft = LocationTypeCheckerHelper.getLocationType(decl.getField(0).getType(),m.getDefaultLocationType());
        assertEquals(LocationType.FAR,ft);
    }

    @Test
    public void varDecl() {
        Model m = assertParseOk("interface I { } { [Somewhere] I i; [Near] I jloc; i = jloc; }",true);
        m.typeCheck();
        assertEquals(LocationType.NEAR,LocationTypeCheckerHelper.getLocationType(getTypeOfFirstAssignment(m),m.getDefaultLocationType()));
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
        assertTypeOk("{ [Near] I i; i = i.m(); }");
    }

    @Test
    public void syncCallOnThis() {
        assertTypeOk("class D { Unit m() { this.m(); } }");
    }
    
    @Test
    public void nullLit() {
        assertTypeOk("{ [Near] I i; i = null; [Far] I j; j = null; }");
    }
    
    @Test
    public void syncCallParam() {
        assertTypeOk("{ [Near] I i; [Far] I j; j = i.n(i); }");
    }

    @Test
    public void newCog() {
        assertTypeOk("{ [Far] I i; i = new cog C(i); }");
    }

    @Test
    public void newObject() {
        assertTypeOk("{ [Near] J i; i = new E(); }");
        assertTypeOk("{ [Somewhere] J i; i = new E(); }");
    }

    @Test
    public void typeMaybe() {
        assertTypeOk("{ [Near] I i; Maybe<[Near] I> m = Just(i); }");
    }
    
    @Test
    public void syncCallOnMaybeThis() {
        assertTypeOk("interface K { Unit m(Maybe<[Near] K> p); } class D implements K { Unit m(Maybe<[Near] K> p) { this.m(Just(this)); } }");
    }

    @Test
    public void typeParamInference() {
        assertTypeOk("{ [Near] I i; Maybe<Maybe<Bool>> m = Nothing; }");
    }
    
    @Test
    public void defaultTyping() {
        assertTypeOk("{ I i; [Far] I f; i = new C(f); }");
    }

    @Test
    public void futureTyping() {
        assertTypeOk("{ I i; [Far] I f; Fut<I> fut; i = new C(f); fut = i!m(); }");
    }
    
    
    
    
    // negative tests:

    
    @Test
    public void typeMaybeError() {
        assertTypeErrorOnly("interface I { } { [Far] I i; Maybe<[Near] I> m = Just(i); }");
    }

    @Test
    public void typeListError() {
        assertTypeError("{ List<[Far] I> list = Nil; [Near] I i; list = Cons(i,list); }");
    }
    
    @Test
    public void assignWrong() {
        assertTypeError("{ [Far] I i; [Near] I j; i = j; }");
    }

    @Test
    public void illegalSyncCall() {
        assertTypeError("{ [Far] I i; i.m(); }");
    }

    @Test
    public void illegalAsyncSyncCall() {
        assertTypeError("{ [Far] I i; i!farM(i); }");
    }
    
    @Test
    public void syncCallWrongParam() {
        assertTypeError("{ [Near] I i; [Far] I j; j = i.n(j); }");
    }
    
    
    private void assertTypeError(String code) {
        assertTypeErrorOnly(INT+code);
    }
    
    private void assertTypeErrorOnly(String code) {
        Model m = assertParseOk(code,true);
        m.setLocationTypingEnabled(true);
        SemanticErrorList e = m.typeCheck();
        assertFalse(e.isEmpty());
    }

    private void assertTypeOk(String code) {
        Model m = assertParseOk(INT+code,true);
        m.setLocationTypingEnabled(true);
        SemanticErrorList e = m.typeCheck();
        assertTrue(e.isEmpty() ? "" : "Found error "+e.get(0).getMessage(),e.isEmpty());
    }
    
    
    @Test
    public void multipleError() {
        Model m = assertParseOk("interface I { } class C { [Far] [Near] I i; }",true);
        ClassDecl decl = getFirstClassDecl(m);
        try {
            LocationTypeCheckerHelper.getLocationType(decl.getField(0).getType(),m.getDefaultLocationType());
            fail("Expected exception");
        } catch(LocationTypeCheckerException e) {
            assertTrue(true);
        }
    }


}
