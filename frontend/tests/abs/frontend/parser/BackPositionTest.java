/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.parser;

import static abs.ABSTest.Config.ALLOW_INCOMPLETE_EXPR;
import static abs.ABSTest.Config.WITH_STD_LIB;
import static org.junit.Assert.assertTrue;
import junit.framework.Assert;

import org.junit.Test;

import beaver.Symbol;

import abs.frontend.FrontendTest;
import abs.frontend.ast.ASTNode;
import abs.frontend.ast.Binary;
import abs.frontend.ast.Block;
import abs.frontend.ast.ClassDecl;
import abs.frontend.ast.CompilationUnit;
import abs.frontend.ast.DataTypeUse;
import abs.frontend.ast.Exp;
import abs.frontend.ast.FnApp;
import abs.frontend.ast.MainBlock;
import abs.frontend.ast.Model;
import abs.frontend.ast.ModuleDecl;
import abs.frontend.ast.PureExp;
import abs.frontend.ast.StringLiteral;
import abs.frontend.ast.VarDecl;
import abs.frontend.ast.VarDeclStmt;

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
    public void testString() {
        assertNodeAtPos("module M;  def String abc() = \"a\";\n" + 
                  "{ String s = \"abc\" + abc(); } ", 2, 15, StringLiteral.class);
    }

    @Test
    public void testString2() {
        assertNodeAtPos("module M;  def String abc() = \"a\";\n" + 
                  "{ String s = \"abc\" + abc(); } ", 2, 24, FnApp.class);
    }
    
    @Test
    public void testMainBlock() {
        assertNodeAtPos("module M; interface I {} { I i; i = null;    }", 1, 45, Block.class);
    }

    private void assertNodeAtPos(String absCode, int line, int col, Class<?> clazz) {
        Model m = null;
        try {
            m = Main.parseString(absCode, false, true);
        } catch (Exception e) {
           e.printStackTrace();

            Assert.fail();
        }
        SourcePosition pos = SourcePosition.findPosition(m.getCompilationUnit(0), line, col);
        if (pos == null)
            assertTrue("Expected to find " + clazz + " at " + line + ":" + col + " but found nothing", false);
        else
            assertTrue("Expected " + clazz + " but found " + pos.getContextNode().getClass(),
                    clazz.isInstance(pos.getContextNode()));
    }

}
