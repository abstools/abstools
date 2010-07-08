package abs.frontend.typesystem;

import static abs.common.StandardLib.STDLIB_STRING;
import static org.junit.Assert.assertEquals;

import org.junit.Test;

import abs.frontend.FrontendTest;
import abs.frontend.ast.ClassDecl;
import abs.frontend.ast.FieldDecl;
import abs.frontend.ast.Model;
import abs.frontend.ast.ReturnStmt;
import abs.frontend.typechecker.UnionType;

public class TypingTest extends FrontendTest {

    @Test
    public void testContextDecl() {
        Model m = assertParseOk("class C implements I { I m() { return this; } } interface I { }");
        ClassDecl d = (ClassDecl)m.getDecl(0);
        ReturnStmt s = (ReturnStmt) d.getMethod(0).getBlock().getStmt(0);
        assertEquals(d,s.getRetExp().getContextDecl());
    }
    
    @Test
    public void testThisTyping() {
        Model m = assertParseOk("class C implements I { I m() { return this; } } interface I { }");
        ClassDecl d = (ClassDecl)m.getDecl(0);
        ReturnStmt s = (ReturnStmt) d.getMethod(0).getBlock().getStmt(0);
        assertEquals(m.getDecl(1),((UnionType)s.getRetExp().getType()).getType(0).getDecl());
        
    }
    
    @Test
    public void testInterfaceType() {
        Model m = assertParseOk("interface I { } { I i; i = i; }");
        assertEquals(m.localLookup("I").getType(),getTypeOfFirstAssignment(m));
        
    }
    
    @Test
    public void testDataTypeBoolLit() {
        Model m = assertParseOk(STDLIB_STRING + "{ Bool i; i = True; }");
        assertEquals(m.getBoolType(),getTypeOfFirstAssignment(m));
    }
    
    @Test
    public void testDataTypeIntLit() {
        Model m = assertParseOk(STDLIB_STRING + "{ Int i; i = 5; }");
        assertEquals(m.getIntType(),getTypeOfFirstAssignment(m));
    }

    @Test
    public void testDataTypeStringLit() {
        Model m = assertParseOk(STDLIB_STRING + "{ String i; i = \"5\"; }");
        assertEquals(m.getStringType(),getTypeOfFirstAssignment(m));
    }
    
    @Test
    public void testLetExp() {
   	 Model m = assertParseOk(STDLIB_STRING + "def Bool f() = let (Bool b) = True in b");
        assertEquals(m.getBoolType(),getFirstFunctionExpr(m).getType());
    }

    @Test
    public void testFnApp() {
   	   Model m = assertParseOk(STDLIB_STRING + "def Bool f() = f()");
       assertEquals(m.getBoolType(),getFirstFunctionExpr(m).getType());
    }


    @Test
    public void testNew() {
        Model m = assertParseOk("interface I {} class C implements I {} { I i; i = new C(); }");
        assertEquals(m.localLookup("I").getType(),((UnionType)getFirstExp(m).getType()).getType(0));
    }
    
    @Test
    public void testFieldUse() {
        Model m = assertParseOk(STDLIB_STRING + " class C { Bool f; Bool m() { return this.f; } }");
        ClassDecl d = (ClassDecl) m.localLookup("C");
        FieldDecl f = d.getField(0);
        ReturnStmt s = (ReturnStmt) d.getMethod(0).getBlock().getStmt(0);
        assertEquals(f.getType(), s.getRetExp().getType());
    }
    
    @Test
    public void testSyncCall() {
        Model m = assertParseOk(STDLIB_STRING + " interface I { Bool m(); } { I i; i.m(); }");
        assertEquals(m.getBoolType(), getFirstExp(m).getType());
    }
    
    @Test
    public void testAsyncCall() {
        Model m = assertParseOk(STDLIB_STRING + " interface I { Bool m(); } { I i; i!m(); }");
        assertEquals(m.getFutType(m.getBoolType()), getFirstExp(m).getType());
    }
}
