package abs.frontend.parser;

import static org.junit.Assert.assertTrue;

import org.junit.Test;

import AST.MethodDecl;
import abs.frontend.FrontendTest;
import abs.frontend.ast.ASTNode;
import abs.frontend.ast.Block;
import abs.frontend.ast.ClassDecl;
import abs.frontend.ast.DataTypeUse;
import abs.frontend.ast.Model;
import abs.frontend.ast.ModuleDecl;
import abs.frontend.ast.VarDecl;

public class BackPositionTest extends FrontendTest {
    @Test
    public void test1() {
        assertNodeAtPos("module M; ", 1, 1, ModuleDecl.class);
    }

    @Test
    public void test2() {
        assertNodeAtPos("module M; class C {    }", 1, 23, ClassDecl.class);
    }

    @Test
    public void test3() {
        assertNodeAtPos("module M;     class C {}", 1, 13, ModuleDecl.class);
    }

    @Test
    public void test4() {
        assertNodeAtPos("module M; class C { C m() {   } }", 1, 30, Block.class);
    }

    @Test
    public void test5() {
        assertNodeAtPos("module M; interface Intf { } class C { C m() { Intf someName; } } ", 1, 52, VarDecl.class);
    }

    @Test
    public void test6() {
        assertNodeAtPos("module M; interface Intf { } class C { C m() { Intf someName; } } ", 1, 51, DataTypeUse.class);
    }

    @Test
    public void testMainBlock() {
        assertNodeAtPos("module M; interface I {} { I i; i = null;    }", 1, 45, Block.class);
    }

    private void assertNodeAtPos(String absCode, int line, int col, Class<?> clazz) {
        Model m = assertParseOk(absCode, false, false);
        SourcePosition pos = SourcePosition.findPosition(m.getCompilationUnit(0), line, col);
        if (pos == null)
            assertTrue("Expected to find " + clazz + " at " + line + ":" + col + " but found nothing", false);
        else
            assertTrue("Expected " + clazz + " but found " + pos.getContextNode().getClass(),
                    clazz.isInstance(pos.getContextNode()));
    }
}
