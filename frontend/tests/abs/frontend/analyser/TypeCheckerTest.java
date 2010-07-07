package abs.frontend.analyser;

import org.junit.Test;

import static org.junit.Assert.*;


import abs.frontend.ast.AssignStmt;
import abs.frontend.ast.Model;
import abs.frontend.typechecker.DataTypeType;
import abs.frontend.typechecker.InterfaceType;
import abs.frontend.typechecker.Type;

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
    
    Type getTypeOfFirstAssignment(Model m) {
        AssignStmt s = (AssignStmt) m.getBlock().getStmts().getChild(0);
        return s.getValue().getType();
    }
    
}
