package org.abs_models.frontend.typesystem;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.assertNull;

import org.abs_models.frontend.FrontendTest;
import org.abs_models.frontend.ast.*;
import org.abs_models.frontend.typechecker.KindedName;
import org.abs_models.frontend.typechecker.nullable.NullableType;
import org.abs_models.frontend.typechecker.nullable.SimpleSet;
import org.junit.Test;

public class NullableTests extends FrontendTest {
    @Test
    public void testMethodDeclNewExp() {
        MethodImpl met = getMethod("interface I { Unit m(); } class C implements I { Unit m() { I i = new C(); i; } }");

        Block b = met.getBlock();
        VarDeclStmt s = (VarDeclStmt) b.getStmt(0);
        VarDecl d = s.getVarDecl();
        ExpressionStmt es = (ExpressionStmt) b.getStmt(1);
        VarOrFieldUse v = (VarOrFieldUse) es.getExp();

        assertEquals(1, b.getStmt(0).nonNull_out().size());
        assertTrue(b.getStmt(0).nonNull_out().contains(d));
        assertTrue(v.nonNull());
        assertEquals(NullableType.NonNull, v.getNullableType());
    }

    @Test
    public void testMethodAssignAccess() {
        MethodImpl met = getMethod("interface I { Unit m(); } class C implements I { Unit m() { I i = new C(); I j; j = i; j; } }");
        Block b = met.getBlock();

        VarDecl d0 = ((VarDeclStmt) b.getStmt(0)).getVarDecl();
        VarDecl d1 = ((VarDeclStmt) b.getStmt(1)).getVarDecl();
        AssignStmt a = (AssignStmt) b.getStmt(2);
        ExpressionStmt es = (ExpressionStmt) b.getStmt(3);

        assertEquals(2, a.nonNull_out().size());
        assertTrue(a.nonNull_out().contains(d0));
        assertTrue(a.nonNull_out().contains(d1));
        assertEquals(NullableType.Nullable, d0.getNullableType());
        assertEquals(NullableType.NonNull, es.getExp().getNullableType());
    }

    @Test
    public void testMethodAssignAs() {
        MethodImpl met = getMethod("interface I { Unit m(); } class C implements I { Unit m() { I i = new C(); I j; j = i as C; j; } }");
        Block b = met.getBlock();

        VarDecl d0 = ((VarDeclStmt) b.getStmt(0)).getVarDecl();
        AssignStmt a = (AssignStmt) b.getStmt(2);
        ExpressionStmt es = (ExpressionStmt) b.getStmt(3);

        assertEquals(1, a.nonNull_out().size());
        assertTrue(a.nonNull_out().contains(d0));
        assertEquals(NullableType.Nullable, es.getExp().getNullableType());
    }

    @Test
    public void testMethodAssignBinary() {
        MethodImpl met = getMethod("interface I { Unit m(); } class C implements I { Unit m() { Int n; n = 1 + 4; n; } }");
        Block b = met.getBlock();

        AssignStmt a = (AssignStmt) b.getStmt(1);
        ExpressionStmt es = (ExpressionStmt) b.getStmt(2);

        assertEquals(0, a.nonNull_out().size());
        assertNull(es.getExp().getNullableType());
    }

    @Test
    public void testMethodAssignCase1() {
        MethodImpl met = getMethod("interface I { Unit m(); } class C implements I { Unit m([NonNull] I i1, [NonNull] I i2, Int n) { I j; j = case n { 0 => i1; 1 => i2; }; j; } }");
        Block b = met.getBlock();

        ParamDecl p0 = met.getMethodSig().getParam(0);
        ParamDecl p1 = met.getMethodSig().getParam(1);
        VarDecl d0 = ((VarDeclStmt) b.getStmt(0)).getVarDecl();
        AssignStmt a = (AssignStmt) b.getStmt(1);
        ExpressionStmt es = (ExpressionStmt) b.getStmt(2);

        assertEquals(3, a.nonNull_out().size());
        assertTrue(a.nonNull_out().contains(p0));
        assertTrue(a.nonNull_out().contains(p1));
        assertTrue(a.nonNull_out().contains(d0));

        assertEquals(NullableType.NonNull, p0.getNullableType());
        assertEquals(NullableType.NonNull, p1.getNullableType());
        assertEquals(NullableType.Nullable, d0.getNullableType());
        assertEquals(NullableType.NonNull, es.getExp().getNullableType());
    }

    @Test
    public void testMethodAssignDataConstructor() {
        MethodImpl met = getMethod("interface I { Unit m(); } class C implements I { Unit m() { List<Int> l; l = Cons(1, Nil); l; } }");
        Block b = met.getBlock();

        VarDeclStmt ds = (VarDeclStmt) b.getStmt(0);
        AssignStmt a = (AssignStmt) b.getStmt(1);
        ExpressionStmt es = (ExpressionStmt) b.getStmt(2);

        assertEquals(0, a.nonNull_out().size());
        assertNull(ds.getVarDecl().getNullableType());
    }

    @Test
    public void testMethodAssignFn1() {
        MethodImpl met = getMethod("def I f(I i) = i; interface I { Unit m(); } class C implements I { Unit m() { I i; i = f(this); } }");
        Block b = met.getBlock();

        AssignStmt a = (AssignStmt) b.getStmt(1);

        assertEquals(0, a.nonNull_out().size());
        assertEquals(NullableType.Nullable, ((FunctionDecl) met.getModuleDecl().getDecl(0)).getNullableType());
    }

    @Test
    public void testMethodAssignFn2() {
        MethodImpl met = getMethod("def [NonNull] I f(I i1, [NonNull] I i2) = if i1 == null then i2 else i1; interface I { Unit m(); } class C implements I { Unit m() { I i; i = f(this, this); i; } }");
        Block b = met.getBlock();

        VarDecl d0 = ((VarDeclStmt) b.getStmt(0)).getVarDecl();
        AssignStmt a = (AssignStmt) b.getStmt(1);
        ExpressionStmt es = (ExpressionStmt) b.getStmt(2);

        assertEquals(1, a.nonNull_out().size());
        assertTrue(a.nonNull_out().contains(d0));
        assertEquals(NullableType.NonNull, ((FunctionDecl) met.getModuleDecl().getDecl(0)).getNullableType());
        assertEquals(NullableType.NonNull, es.getExp().getNullableType());
    }

    @Test
    public void testMethodAssignFn3() {
        MethodImpl met = getMethod("def [NonNull] I f(I i1, [NonNull] I i2) = if i1 == null then i2 else i1; interface I { Unit m(); } class C implements I { Unit m() { I i; i = f(this, this); } }");
        Block b = met.getBlock();

        VarDecl d0 = ((VarDeclStmt) b.getStmt(0)).getVarDecl();
        AssignStmt a = (AssignStmt) b.getStmt(1);

        assertEquals(1, a.nonNull_out().size());
        assertTrue(a.nonNull_out().contains(d0));
    }

    @Test
    public void testMethodAssignImplements() {
        MethodImpl met = getMethod("interface I { Unit m(); } class C implements I { Unit m() { I i = new C(); Bool b; b = i implements C; } }");
        Block b = met.getBlock();

        VarDecl d0 = ((VarDeclStmt) b.getStmt(0)).getVarDecl();
        AssignStmt a = (AssignStmt) b.getStmt(2);

        assertEquals(1, a.nonNull_out().size());
        assertTrue(a.nonNull_out().contains(d0));
        assertNull(a.getValue().getNullableType());
    }

    @Test
    public void testMethodAssignLet1() {
        MethodImpl met = getMethod("interface I { Unit m(); } class C implements I { Unit m() { I i = new C(); I j; j = let I v = i in v; } }");
        Block b = met.getBlock();

        VarDecl d0 = ((VarDeclStmt) b.getStmt(0)).getVarDecl();
        VarDecl d1 = ((VarDeclStmt) b.getStmt(1)).getVarDecl();
        AssignStmt a = (AssignStmt) b.getStmt(2);

        assertEquals(2, a.nonNull_out().size());
        assertTrue(a.nonNull_out().contains(d0));
        assertTrue(a.nonNull_out().contains(d1));
    }

    @Test
    public void testMethodAssignLet2() {
        MethodImpl met = getMethod("interface I { Unit m(); } class C implements I { Unit m(I i) { I j; j = let I v = i in v; } }");
        Block b = met.getBlock();

        AssignStmt a = (AssignStmt) b.getStmt(1);

        assertEquals(0, a.nonNull_out().size());
    }

    @Test
    public void testMethodAssignLiteral() {
        MethodImpl met = getMethod("interface I { Unit m(); } class C implements I { Unit m() { Int n; n = 2; } }");
        Block b = met.getBlock();

        AssignStmt a = (AssignStmt) b.getStmt(1);

        assertEquals(0, a.nonNull_out().size());
    }

    @Test
    public void testMethodAssignNull() {
        MethodImpl met = getMethod("interface I { Unit m(); } class C implements I { Unit m(I i) { I i = new C(); i = null; } }");
        Block b = met.getBlock();

        VarDecl d0 = ((VarDeclStmt) b.getStmt(0)).getVarDecl();
        AssignStmt a = (AssignStmt) b.getStmt(1);

        assertEquals(1, a.nonNull_in().size());
        assertTrue(a.nonNull_in().contains(d0));
        assertEquals(0, a.nonNull_out().size());
    }

    @Test
    public void testMethodAssignParFn1() {
        MethodImpl met = getMethod("def I p(f)(I i) = i; interface I { Unit m(); } class C implements I { Unit m() { I i; i = p(toString)(this); } }");
        Block b = met.getBlock();

        AssignStmt a = (AssignStmt) b.getStmt(1);

        assertEquals(0, a.nonNull_out().size());
    }

    @Test
    public void testMethodAssignParFn2() {
        MethodImpl met = getMethod("def [NonNull] I p(f)(I i1, [NonNull] I i2) = if i1 == null then i2 else i1; interface I { Unit m(); } class C implements I { Unit m() { I i; i = p(toString)(this, this); } }");
        Block b = met.getBlock();

        VarDecl d0 = ((VarDeclStmt) b.getStmt(0)).getVarDecl();
        AssignStmt a = (AssignStmt) b.getStmt(1);

        assertEquals(1, a.nonNull_out().size());
        assertTrue(a.nonNull_out().contains(d0));
    }

    @Test
    public void testMethodAssignThis() {
        MethodImpl met = getMethod("interface I { Unit m(I i); } class C implements I { Unit m(I i) { i = this; } }");

        ParamDecl p = met.getMethodSig().getParam(0);
        Block b = met.getBlock();

        SimpleSet<VarOrFieldDecl> nonNull = b.getStmt(0).nonNull_out();
        assertTrue(nonNull.contains(p));
        assertEquals(1, nonNull.size());
    }

    @Test
    public void testMethodAssignUnary() {
        MethodImpl met = getMethod("interface I { Unit m(); } class C implements I { Unit m() { Int n; n = -4; } }");
        Block b = met.getBlock();

        AssignStmt a = (AssignStmt) b.getStmt(1);

        assertEquals(0, a.nonNull_out().size());
    }

    @Test
    public void testMethodAssignCall1() {
        MethodImpl met = getMethod("interface I { I m(); } class C implements I { Unit m(I i) { I j; j = i.m(); } }");
        Block b = met.getBlock();

        assertEquals(0, b.getStmt(1).nonNull_out().size());
    }

    @Test
    public void testMethodAssignCall2() {
        MethodImpl met = getMethod("interface I { [NonNull] I m(); } class C implements I { Unit m(I i) { I j; j = i.m(); } }");
        Block b = met.getBlock();

        VarDecl d0 = ((VarDeclStmt) b.getStmt(0)).getVarDecl();

        assertEquals(1, b.getStmt(1).nonNull_out().size());
        assertTrue(b.getStmt(1).nonNull_out().contains(d0));
    }

    @Test
    public void testMethodAssignAsyncCall1() {
        MethodImpl met = getMethod("interface I { I m(); } class C implements I { Unit m(I i) { Fut<I> f; f = i!m(); } }");
        Block b = met.getBlock();
        assertEquals(0, b.getStmt(1).nonNull_out().size());
    }

    @Test
    public void testMethodAssignAsyncCall2() {
        MethodImpl met = getMethod("interface I { [NonNull] I m(); } class C implements I { Unit m(I i) { Fut<I> f; f = i!m(); } }");
        Block b = met.getBlock();

        VarDecl d0 = ((VarDeclStmt) b.getStmt(0)).getVarDecl();

        assertEquals(1, b.getStmt(1).nonNull_out().size());
        assertTrue(b.getStmt(1).nonNull_out().contains(d0));
    }

    @Test
    public void testMethodAssignGet1() {
        MethodImpl met = getMethod("interface I { I m(); } class C implements I { Unit m(I i) { Fut<I> f; f = i!m(); I j; j = f.get; } }");
        Block b = met.getBlock();

        assertEquals(0, b.getStmt(3).nonNull_out().size());
    }

    @Test
    public void testMethodAssignGet2() {
        MethodImpl met = getMethod("interface I { [NonNull] I m(); } class C implements I { Unit m(I i) { Fut<I> f; f = i!m(); I j; j = f.get; } }");
        Block b = met.getBlock();

        VarDecl d0 = ((VarDeclStmt) b.getStmt(0)).getVarDecl();
        VarDecl d1 = ((VarDeclStmt) b.getStmt(2)).getVarDecl();

        assertEquals(2, b.getStmt(3).nonNull_out().size());
        assertTrue(b.getStmt(3).nonNull_out().contains(d0));
        assertTrue(b.getStmt(3).nonNull_out().contains(d1));
    }

    @Test
    public void testMethodAssignNewExp() {
        MethodImpl met = getMethod("interface I { Unit m(I i); } class C implements I { Unit m(I i) { i = new C(); } }");

        ParamDecl p = met.getMethodSig().getParam(0);
        Block b = met.getBlock();

        SimpleSet<VarOrFieldDecl> nonNull = b.getStmt(0).nonNull_out();
        assertTrue(nonNull.contains(p));
        assertEquals(1, nonNull.size());
        assertEquals(NullableType.Nullable, p.getNullableType());
    }

    @Test
    public void testMethodAssignOriginal() {
        // TODO
    }

    @Test
    public void testMethodSecondCall() {
        MethodImpl met = getMethod("interface I { Unit m(I i); } class C implements I { Unit m(I i) { i.m(this); } }");

        ParamDecl p = met.getMethodSig().getParam(0);
        Block b = met.getBlock();

        SimpleSet<VarOrFieldDecl> nonNull1 = b.getStmt(0).nonNull_in();
        SimpleSet<VarOrFieldDecl> nonNull2 = b.getStmt(0).nonNull_out();
        assertTrue(nonNull1.isEmpty());
        assertTrue(nonNull2.contains(p));
    }

    @Test
    public void testMethodAssert() {
        MethodImpl met = getMethod("interface I { Unit m(I i); } class C implements I { Unit m(I i) { assert i != null; skip; } }");

        ParamDecl p = met.getMethodSig().getParam(0);
        Block b = met.getBlock();
        Stmt skip = b.getStmt(1);

        SimpleSet<VarOrFieldDecl> nonNull1 = b.getStmt(0).nonNull_in();
        SimpleSet<VarOrFieldDecl> nonNull2 = b.getStmt(0).nonNull_out();
        assertTrue(nonNull1.isEmpty());
        assertTrue(nonNull2.isEmpty());
        assertTrue(skip.nonNull_in().contains(p));
    }

    @Test
    public void testMethodAssertCatch1() {
        MethodImpl met = getMethod("interface I { Unit m(I i); } class C implements I { Unit m(I i) { try { assert i == null; } catch { AssertionFailException => skip; } } }");
        ParamDecl p = met.getMethodSig().getParam(0);
        Block b = met.getBlock();
        TryCatchFinallyStmt t = (TryCatchFinallyStmt) b.getStmt(0);
        CaseBranchStmt cs = t.getCatch(0);
        Stmt skip = cs.getRight().getStmt(0);

        assertEquals(1, skip.nonNull_out().size());
        assertTrue(skip.nonNull_out().contains(p));
    }

    @Test
    public void testMethodAssertCatch2() {
        MethodImpl met = getMethod("interface I { Unit m(I i); } class C implements I { Unit m(I i) { try { assert i != null; } catch { AssertionFailException => skip; } } }");
        Block b = met.getBlock();
        TryCatchFinallyStmt t = (TryCatchFinallyStmt) b.getStmt(0);
        CaseBranchStmt cs = t.getCatch(0);
        Stmt skip = cs.getRight().getStmt(0);

        assertEquals(0, skip.nonNull_out().size());
    }

    @Test
    public void testMethodIfCondition() {
        MethodImpl met = getMethod("interface I { Unit m(I i); } class C implements I { Unit m(I i) { if (i != null) skip; } }");

        ParamDecl p = met.getMethodSig().getParam(0);
        Block b = met.getBlock();

        IfStmt ifStmt = (IfStmt) b.getStmt(0);

        SimpleSet<VarOrFieldDecl> nonNull = ifStmt.getThen().nonNull_in();
        assertTrue(nonNull.contains(p));
    }

    @Test
    public void testMethodIfInvertedCondition() {
        MethodImpl met = getMethod("interface I { Unit m(I i); } class C implements I { Unit m(I i) { if (i == null) skip; else skip; } }");

        ParamDecl p = met.getMethodSig().getParam(0);
        Block b = met.getBlock();

        IfStmt ifStmt = (IfStmt) b.getStmt(0);

        SimpleSet<VarOrFieldDecl> nonNull = ifStmt.getElse().nonNull_in();
        assertTrue(nonNull.contains(p));
    }

    @Test
    public void testMethodAnnotatedParam() {
        MethodImpl met = getMethod("interface I { Unit m(I i); } class C implements I { Unit m([NonNull] I i) { skip; } }");
        ParamDecl p = met.getMethodSig().getParam(0);
        Block b = met.getBlock();

        assertTrue(met.entry().nonNull_out().contains(p));
        assertEquals(1, met.entry().nonNull_out().size());
        assertTrue(b.getStmt(0).nonNull_in().contains(p));
    }

    @Test
    public void testAfterIf() {
        MethodImpl met = getMethod("interface I { Unit m(I i); } class C implements I { Unit m(I i) { I j; if (i == null) { j = new C(); } else { j = i; } skip; } }");
        Block b = met.getBlock();

        VarDecl d = ((VarDeclStmt) b.getStmt(0)).getVarDecl();
        Stmt s = b.getStmt(2);

        assertEquals(1, s.nonNull_in().size());
        assertTrue(s.nonNull_out().contains(d));
    }

    @Test
    public void testCatch() {
        MethodImpl met = getMethod("interface I { Unit m(); } class C implements I { Unit m([NonNull] I i1, [NonNull] I i2, Int n) { I j; try { j = case n { 0 => i1; 1 => i2; }; } catch { PatternMatchFailException => skip; } } }");
        Block b = met.getBlock();

        ParamDecl p0 = met.getMethodSig().getParam(0);
        ParamDecl p1 = met.getMethodSig().getParam(1);
        TryCatchFinallyStmt t = (TryCatchFinallyStmt) b.getStmt(1);
        CaseBranchStmt cs = t.getCatch(0);
        Stmt skip = cs.getRight().getStmt(0);

        assertEquals(2, skip.nonNull_out().size());
        assertTrue(skip.nonNull_out().contains(p0));
        assertTrue(skip.nonNull_out().contains(p1));
    }

    @Test
    public void testWhile1() {
        MethodImpl met = getMethod("interface I { Unit m(); } class C implements I { Unit m([NonNull] I i, Int n) { while (n > 0) { n = n - 1; } skip;  } }");
        Block b = met.getBlock();

        ParamDecl p = met.getMethodSig().getParam(0);
        WhileStmt w = (WhileStmt) b.getStmt(0);
        Block body = w.getBody();
        Stmt s = body.getStmt(0);
        Stmt skip = b.getStmt(1);

        assertEquals(1, s.nonNull_out().size());
        assertTrue(s.nonNull_out().contains(p));

        assertEquals(1, skip.nonNull_out().size());
        assertTrue(skip.nonNull_out().contains(p));
    }

    @Test
    public void testWhile2() {
        MethodImpl met = getMethod("interface I { Unit m(); } class C implements I { Unit m(I i, Int n) { while (n > 0) { i = new C(); } skip;  } }");
        Block b = met.getBlock();

        ParamDecl p = met.getMethodSig().getParam(0);
        WhileStmt w = (WhileStmt) b.getStmt(0);
        Block body = w.getBody();
        Stmt s = body.getStmt(0);
        Stmt skip = b.getStmt(1);

        assertEquals(1, s.nonNull_out().size());
        assertTrue(s.nonNull_out().contains(p));

        assertEquals(0, skip.nonNull_out().size());
    }

    @Test
    public void testWhile3() {
        MethodImpl met = getMethod("interface I { Unit m(); } class C implements I { Unit m(I i, Int n) { while (i == null) { skip; } skip;  } }");
        Block b = met.getBlock();

        ParamDecl p = met.getMethodSig().getParam(0);
        WhileStmt w = (WhileStmt) b.getStmt(0);
        Block body = w.getBody();
        Stmt s = body.getStmt(0);
        Stmt skip = b.getStmt(1);

        assertEquals(0, s.nonNull_out().size());


        assertEquals(1, skip.nonNull_out().size());
        assertTrue(skip.nonNull_out().contains(p));
    }

    @Test
    public void testWhile4() {
        MethodImpl met = getMethod("interface I { Unit m(); } class C implements I { Unit m(I i, Int n) { while (i != null) { skip; } skip;  } }");
        Block b = met.getBlock();

        ParamDecl p = met.getMethodSig().getParam(0);
        WhileStmt w = (WhileStmt) b.getStmt(0);
        Block body = w.getBody();
        Stmt s = body.getStmt(0);
        Stmt skip = b.getStmt(1);

        assertEquals(1, s.nonNull_out().size());
        assertTrue(s.nonNull_out().contains(p));

        assertEquals(0, skip.nonNull_out().size());
    }

    @Test
    public void testClassParamInitB() {
        ClassDecl c = getClass("interface I {} class C(I i1, [NonNull] I i2, Int n) implements I { { skip; } }");

        InitBlock i = c.getInitBlock();
        Stmt skip = i.getStmt(0);
        ParamDecl p0 = c.getParam(0);
        ParamDecl p1 = c.getParam(1);
        ParamDecl p2 = c.getParam(2);

        assertEquals(NullableType.Nullable, p0.getNullableType());
        assertEquals(NullableType.NonNull, p1.getNullableType());
        assertNull(p2.getNullableType());
        assertEquals(1, skip.nonNull_in().size());
        assertTrue(skip.nonNull_in().contains(p1));
    }

    @Test
    public void testClassParamMethod() {
        MethodImpl met = getMethod("interface I {} class C(I i1, [NonNull] I i2, Int n) implements I { Unit m() { skip; } }");

        ClassDecl c = (ClassDecl) met.getParent().getParent();

        ParamDecl p0 = c.getParam(0);
        ParamDecl p1 = c.getParam(1);
        ParamDecl p2 = c.getParam(2);
        Stmt skip = met.getBlock().getStmt(0);

        assertEquals(NullableType.Nullable, p0.getNullableType());
        assertEquals(NullableType.NonNull, p1.getNullableType());
        assertNull(p2.getNullableType());
        assertEquals(1, skip.nonNull_in().size());
        assertTrue(skip.nonNull_in().contains(p1));
    }

    @Test
    public void testClassFieldInitB() {
        ClassDecl c = getClass("interface I {} class C implements I { I i1; [NonNull] I i2; Int n = 2; { skip; } }");

        InitBlock i = c.getInitBlock();
        Stmt skip = i.getStmt(0);
        FieldDecl f0 = c.getField(0);
        FieldDecl f1 = c.getField(1);
        FieldDecl f2 = c.getField(2);

        assertEquals(NullableType.Nullable, f0.getNullableType());
        assertEquals(NullableType.NonNull, f1.getNullableType());
        assertNull(f2.getNullableType());
        assertEquals(0, skip.nonNull_in().size());
    }

    @Test
    public void testClassFieldMethod() {
        MethodImpl met = getMethod("interface I {} class C implements I { I i1; [NonNull] I i2; Int n = 2; Unit m() { skip; } }");

        ClassDecl c = (ClassDecl) met.getParent().getParent();

        FieldDecl f0 = c.getField(0);
        FieldDecl f1 = c.getField(1);
        FieldDecl f2 = c.getField(2);
        Stmt skip = met.getBlock().getStmt(0);

        assertEquals(NullableType.Nullable, f0.getNullableType());
        assertEquals(NullableType.NonNull, f1.getNullableType());
        assertNull(f2.getNullableType());
        assertEquals(1, skip.nonNull_in().size());
        assertTrue(skip.nonNull_in().contains(f1));
    }

    static private ClassDecl getClass(String prog) {
        Model m = assertParse(prog);
        m.typeCheck();
        return (ClassDecl) getTestModule(m).lookup(new KindedName(KindedName.Kind.CLASS, "UnitTest.C"));
    }

    static private MethodImpl getMethod(String prog) {
        return getClass(prog).getMethod(0);
    }

    static private ModuleDecl getTestModule(Model m) {
        ModuleDecl md = m.lookupModule("UnitTest");
        assertNotNull("Module UnitTest not found", md);
        return md;
    }
}
