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
import abs.frontend.ast.DeltaID;
import abs.frontend.ast.Deltaspec;
import abs.frontend.ast.Exp;
import abs.frontend.ast.FnApp;
import abs.frontend.ast.MainBlock;
import abs.frontend.ast.Model;
import abs.frontend.ast.ModuleDecl;
import abs.frontend.ast.PureExp;
import abs.frontend.ast.StringLiteral;
import abs.frontend.ast.VarDecl;
import abs.frontend.ast.VarDeclStmt;
import abs.frontend.ast.VarUse;

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

    @Test
    public void testListFunction() {
        assertNodeAtPos("module M; { Int abc = 2; List<Int> ints = list[1, abc, 3]; }", 1, 51, VarUse.class);
    }
    

    @Test
    public void testFuncDef() {
        assertNodeAtPos("module M; def Int foo(Int abc) = abc + 1;", 1, 34, VarUse.class);
    }
    
    @Test
    public void testDeltaId() {
        assertNodeAtPos("module M;\n" +
        		"class C {}\n" +
        		"delta D1; modifies class C { adds Unit m() {} }\n" +
        		"delta D2; modifies class C { modifies Unit m() { original(); D1.original(); } }", 4, 63, DeltaID.class);
    }
    
    @Test
    public void testDeltaClause() {
        assertNodeAtPos("module Bla; productline PL; features X; delta KX when X;", 1, 48, Deltaspec.class);
    }
    
    
    
    
    
    private void assertNodeAtPos(String absCode, int line, int col, Class<?> clazz) {
        assertParseOk(absCode, WITH_STD_LIB);
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
