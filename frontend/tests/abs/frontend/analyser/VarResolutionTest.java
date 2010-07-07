package abs.frontend.analyser;

import org.junit.Test;

import static org.junit.Assert.*;

import abs.frontend.ast.CaseBranch;
import abs.frontend.ast.CaseExp;
import abs.frontend.ast.Exp;
import abs.frontend.ast.FunctionDecl;
import abs.frontend.ast.Model;
import abs.frontend.ast.NegExp;
import abs.frontend.ast.PatternVarDecl;
import abs.frontend.ast.PureExp;
import abs.frontend.ast.VarDecl;
import abs.frontend.ast.VarUse;

public class VarResolutionTest extends AnalyserTest {
    @Test
    public void testLocalVar() {
        Exp e = getFirstExp("interface I { } { I i; i = i; }");
        VarUse u = (VarUse) e;
        VarDecl decl = (VarDecl) u.getDecl();
        assertEquals("i",decl.getName());
    }
    
    @Test
    public void testPatternVar() {
        Model m = assertParseOk("data Bool { False; True; } def Bool f(Bool b) = case b { True => False; x => ~x; }");
        FunctionDecl f = (FunctionDecl) m.getDecls().getChild(1);
        CaseExp ce = (CaseExp) f.getFunDef();
        CaseBranch b = ce.getBranch(1);
        NegExp ne = (NegExp) b.getRight();
        VarUse v = (VarUse) ne.getOperand();
        PatternVarDecl decl = (PatternVarDecl) v.getDecl();
        assertEquals("x",decl.getName());
        
        
        
    }

}
