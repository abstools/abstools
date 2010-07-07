package abs.frontend.analyser;

import org.junit.Test;

import static org.junit.Assert.*;

import abs.frontend.ast.CaseBranch;
import abs.frontend.ast.CaseExp;
import abs.frontend.ast.Exp;
import abs.frontend.ast.FunctionDecl;
import abs.frontend.ast.LetExp;
import abs.frontend.ast.Model;
import abs.frontend.ast.NegExp;
import abs.frontend.ast.ParamDecl;
import abs.frontend.ast.PatternVarDecl;
import abs.frontend.ast.VarDecl;
import abs.frontend.ast.VarOrFieldDecl;
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
        NegExp ne = (NegExp) getFirstCaseExpr(m);
        VarUse v = (VarUse) ne.getOperand();
        PatternVarDecl decl = (PatternVarDecl) v.getDecl();
        assertEquals("x",decl.getName());
    }

    private Exp getFirstCaseExpr(Model m) {
        CaseExp ce = (CaseExp) getFirstFunctionExpr(m);
        CaseBranch b = ce.getBranch(1);
        return b.getRight();
    }

    private Exp getFirstFunctionExpr(Model m) {
        FunctionDecl f = (FunctionDecl) m.getDecls().getChild(1);
        return f.getFunDef();
    }
    
    @Test
    public void testFunctionParam() {
        Model m = assertParseOk("data Bool { False; True; } def Bool f(Bool b) = b");
        VarUse u = (VarUse) getFirstFunctionExpr(m);
        ParamDecl d = (ParamDecl) u.getDecl();
        assertEquals("b", d.getName());

    }
    
    @Test
    public void testLetExp() {
        Model m = assertParseOk("data Bool { False; True; } def Bool f(Bool b) = let (Bool x) = b in x");
        LetExp e = (LetExp) getFirstFunctionExpr(m);
        VarOrFieldDecl decl = e.getVar();
        VarUse u = (VarUse) e.getExp();
        assertEquals(decl, u.getDecl());

    }

    @Test
    public void testNestedLetExp() {
        Model m = assertParseOk("data Bool { False; True; } def Bool f(Bool b) = let (Bool x) = let (Bool y) = b in y in x");
        LetExp e = (LetExp) getFirstFunctionExpr(m);
        VarOrFieldDecl decl = e.getVar();
        VarUse u = (VarUse) e.getExp();
        assertEquals(decl, u.getDecl());

    }

    @Test
    public void testNestedLetExp2() {
        Model m = assertParseOk("data Bool { False; True; } def Bool f(Bool b) = let (Bool x) = let (Bool x) = b in x in x");
        LetExp e = (LetExp) getFirstFunctionExpr(m);
        VarOrFieldDecl decl = e.getVar();
        VarUse u = (VarUse) e.getExp();
        assertEquals(decl, u.getDecl());

    }

    @Test
    public void testNestedLetExp3() {
        Model m = assertParseOk("data Bool { False; True; } def Bool f(Bool b) = let (Bool x) = b in let (Bool y) = b in x");
        LetExp e = (LetExp) getFirstFunctionExpr(m);
        LetExp e2 = (LetExp) e.getExp();
        VarOrFieldDecl decl = e.getVar();
        VarUse u = (VarUse) e2.getExp();
        assertEquals(decl, u.getDecl());
    }
    
    @Test
    public void testNestedLetExp4() {
        Model m = assertParseOk("data Bool { False; True; } def Bool f(Bool b) = let (Bool x) = b in let (Bool x) = b in x");
        LetExp e = (LetExp) getFirstFunctionExpr(m);
        LetExp e2 = (LetExp) e.getExp();
        VarOrFieldDecl decl = e2.getVar();
        VarUse u = (VarUse) e2.getExp();
        assertEquals(decl, u.getDecl());
    }
    
    @Test
    public void testNestedLetExp5() {
        Model m = assertParseOk("data Bool { False; True; } def Bool f(Bool b) = let (Bool x) = b in let (Bool x) = x in x");
        LetExp e = (LetExp) getFirstFunctionExpr(m);
        LetExp e2 = (LetExp) e.getExp();
        VarOrFieldDecl decl = e.getVar();
        VarUse u = (VarUse) e2.getVal();
        assertEquals(decl, u.getDecl());
    }


}
