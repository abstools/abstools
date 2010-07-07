package abs.frontend.analyser;

import org.junit.Test;

import static org.junit.Assert.*;


import abs.frontend.ast.AssignStmt;
import abs.frontend.ast.Exp;
import abs.frontend.ast.LetExp;
import abs.frontend.ast.Model;
import abs.frontend.ast.NewExp;
import abs.frontend.typechecker.DataTypeType;
import abs.frontend.typechecker.InterfaceType;
import abs.frontend.typechecker.Type;
import abs.frontend.typechecker.UnionType;

public class TypeCheckerTest extends AnalyserTest {

    @Test
    public void testInterfaceType() {
        Model m = assertParseOk("interface I { } { I i; i = i; }");
        InterfaceType t = (InterfaceType) getTypeOfFirstAssignment(m);
        assertEquals(m.getDecls().getChild(0),t.getDecl());
        
    }
    
    @Test
    public void testDataTypeBoolLit() {
        Model m = assertParseOk("data Bool { True; False; } { Bool i; i = True; }");
        DataTypeType t = (DataTypeType) getTypeOfFirstAssignment(m);
        assertEquals(m.getDecls().getChild(0),t.getDecl());
    }
    
    @Test
    public void testDataTypeIntLit() {
        Model m = assertParseOk("data Int { } { Int i; i = 5; }");
        DataTypeType t = (DataTypeType) getTypeOfFirstAssignment(m);
        assertEquals(m.getDecls().getChild(0),t.getDecl());
    }

    @Test
    public void testDataTypeStringLit() {
        Model m = assertParseOk("data String { } { String i; i = \"5\"; }");
        DataTypeType t = (DataTypeType) getTypeOfFirstAssignment(m);
        assertEquals(m.getDecls().getChild(0),t.getDecl());
    }
    
    @Test
    public void testLetExp() {
   	 Model m = assertParseOk("data Bool { False; True; } def Bool f() = let (Bool b) = True in b");
       LetExp e = (LetExp) getFirstFunctionExpr(m);
        assertEquals(m.getDecls().getChild(0),((DataTypeType)e.getType()).getDecl());
    }

    @Test
    public void testFnApp() {
   	 Model m = assertParseOk("data Bool { False; True; } def Bool f() = f()");
       Exp e = getFirstFunctionExpr(m);
       assertEquals(m.getDecls().getChild(0),((DataTypeType)e.getType()).getDecl());
    }


    @Test
    public void testNew() {
        Model m = assertParseOk("interface I {} class C implements I {} { I i; i = new C(); }");
        NewExp e = (NewExp) getFirstExp(m);
        assertEquals(m.getDecls().getChild(0),((UnionType)e.getType()).getTypes().get(0).getDecl());
    }
    
    Type getTypeOfFirstAssignment(Model m) {
        AssignStmt s = (AssignStmt) m.getBlock().getStmts().getChild(0);
        return s.getValue().getType();
    }
    
}
