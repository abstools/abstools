package abs.frontend.analyser;

import org.junit.Test;

import static org.junit.Assert.*;

import abs.frontend.FrontendTest;
import abs.frontend.ast.AssignStmt;
import abs.frontend.ast.CaseExp;
import abs.frontend.ast.ClassDecl;
import abs.frontend.ast.ConstructorPattern;
import abs.frontend.ast.Exp;
import abs.frontend.ast.FieldDecl;
import abs.frontend.ast.FieldUse;
import abs.frontend.ast.LetExp;
import abs.frontend.ast.Model;
import abs.frontend.ast.ModuleDecl;
import abs.frontend.ast.NegExp;
import abs.frontend.ast.ParamDecl;
import abs.frontend.ast.PatternVar;
import abs.frontend.ast.PatternVarDecl;
import abs.frontend.ast.ReturnStmt;
import abs.frontend.ast.VarDecl;
import abs.frontend.ast.VarOrFieldDecl;
import abs.frontend.ast.VarUse;
import abs.frontend.typechecker.KindedName;
import abs.frontend.typechecker.KindedName.Kind;

public class VarResolutionTest extends FrontendTest {
    @Test
    public void testLocalVar() {
        Exp e = getFirstExp("interface I { } { I i = i; }");
        VarUse u = (VarUse) e;
        VarDecl decl = (VarDecl) u.getDecl();
        assertEquals("i", decl.getName());
    }

    @Test
    public void testPatternVar() {
        Model m = assertParseOkStdLib(" def Bool f(Bool b) = case b { True => False; x => ~x; };");
        NegExp ne = (NegExp) getSecondCaseExpr(m);
        VarUse v = (VarUse) ne.getOperand();
        PatternVarDecl decl = (PatternVarDecl) v.getDecl();
        assertEquals("x", decl.getName());
    }

    @Test
    public void testNestedPatternVar() {
        Model m = assertParseOkStdLib("data Foo = Bar(Bool); def Bool m(Foo f) = case f { Bar(y) => y; };");
        VarUse v = (VarUse) getFirstCaseExpr(m);
        ConstructorPattern p = (ConstructorPattern) getFirstCasePattern(m);
        PatternVarDecl decl = ((PatternVar) p.getParam(0)).getVar();
        assertEquals("y", decl.getName());
    }

    @Test
    public void testFunctionParam() {
        Model m = assertParseOkStdLib(" def Bool f(Bool b) = b;");
        VarUse u = (VarUse) getFirstFunctionExpr(m);
        ParamDecl d = (ParamDecl) u.getDecl();
        assertEquals("b", d.getName());

    }

    @Test
    public void testLetExp() {
        Model m = assertParseOkStdLib(" def Bool f(Bool b) = let (Bool x) = b in x;");
        LetExp e = (LetExp) getFirstFunctionExpr(m);
        VarOrFieldDecl decl = e.getVar();
        VarUse u = (VarUse) e.getExp();
        assertEquals(decl, u.getDecl());

    }

    @Test
    public void testNestedLetExp() {
        Model m = assertParseOkStdLib(" def Bool f(Bool b) = let (Bool x) = let (Bool y) = b in y in x;");
        LetExp e = (LetExp) getFirstFunctionExpr(m);
        VarOrFieldDecl decl = e.getVar();
        VarUse u = (VarUse) e.getExp();
        assertEquals(decl, u.getDecl());

    }

    @Test
    public void testNestedLetExp2() {
        Model m = assertParseOkStdLib(" def Bool f(Bool b) = let (Bool x) = let (Bool x) = b in x in x;");
        LetExp e = (LetExp) getFirstFunctionExpr(m);
        VarOrFieldDecl decl = e.getVar();
        VarUse u = (VarUse) e.getExp();
        assertEquals(decl, u.getDecl());

    }

    @Test
    public void testNestedLetExp3() {
        Model m = assertParseOkStdLib(" def Bool f(Bool b) = let (Bool x) = b in let (Bool y) = b in x;");
        LetExp e = (LetExp) getFirstFunctionExpr(m);
        LetExp e2 = (LetExp) e.getExp();
        VarOrFieldDecl decl = e.getVar();
        VarUse u = (VarUse) e2.getExp();
        assertEquals(decl, u.getDecl());
    }

    @Test
    public void testNestedLetExp4() {
        Model m = assertParseOkStdLib(" def Bool f(Bool b) = let (Bool x) = b in let (Bool x) = b in x;");
        LetExp e = (LetExp) getFirstFunctionExpr(m);
        LetExp e2 = (LetExp) e.getExp();
        VarOrFieldDecl decl = e2.getVar();
        VarUse u = (VarUse) e2.getExp();
        assertEquals(decl, u.getDecl());
    }

    @Test
    public void testNestedLetExp5() {
        Model m = assertParseOkStdLib("def Bool f(Bool b) = let (Bool x) = b in let (Bool x) = x in x;");
        LetExp e = (LetExp) getFirstFunctionExpr(m);
        LetExp e2 = (LetExp) e.getExp();
        VarOrFieldDecl decl = e.getVar();
        VarUse u = (VarUse) e2.getVal();
        assertEquals(decl, u.getDecl());
    }

    @Test
    public void testFieldUse() {
        Model m = assertParseOkStdLib(" class C { Bool f; Bool m() { return this.f; } }");
        ClassDecl d = (ClassDecl) getTestModule(m).lookup(new KindedName(Kind.CLASS, "UnitTest.C"));
        FieldDecl f = d.getField(0);
        ReturnStmt s = (ReturnStmt) d.getMethod(0).getBlock().getStmt(0);
        assertEquals(f, ((FieldUse) s.getRetExp()).getDecl());
    }

    private ModuleDecl getTestModule(Model m) {
        return m.getCompilationUnit(1).getModuleDecl(0);
    }

}
