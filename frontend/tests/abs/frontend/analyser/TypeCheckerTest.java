package abs.frontend.analyser;

import org.junit.Test;

import static org.junit.Assert.*;


import abs.frontend.ast.AssignStmt;
import abs.frontend.ast.Model;
import abs.frontend.ast.Stmt;
import abs.frontend.ast.Type;

public class TypeCheckerTest extends AnalyserTest {

    @Test
    public void test1() {
        //Type t = getTypeOfFirstAssignment("interface I { } { I i; i = i; }");
        
    }
    
    Type getTypeOfFirstAssignment(String absCode) {
        Model m = assertParseOk(absCode);
        AssignStmt s = (AssignStmt) m.getBlock().getStmts().getChild(0);
        return s.getValue().getType();
    }
    
}
