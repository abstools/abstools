/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.frontend.parser;

import static org.abs_models.ABSTest.Config.*;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import org.abs_models.frontend.ast.*;
import org.junit.Test;

import org.abs_models.frontend.FrontendTest;

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
        // do not check for InterfaceTypeUse since if we skip type inference it might be UnresolvedTypeUse
        assertNodeAtPos("module M; interface Intf { } class C { C m() { Intf someName; } } ", 1, 51, TypeUse.class);
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

    @Test
    public void testSourcePosLoop() {
        assertNodeAtPos("module Foo; type InKeyType = String; type InValueType = List<String>; type OutKeyType = String; type OutValueType = Int; interface IMap {  List<Pair<OutKeyType, OutValueType>> invokeMap(InKeyType key, InValueType value);}"
                , 1, 170, UnresolvedTypeUse.class);
    }

    private void assertNodeAtPos(String absCode, int line, int col, Class<?> clazz) {
        Model m = assertParseOk(absCode, WITHOUT_MODULE_NAME);
        SourcePosition pos = SourcePosition.findPosition(m.getCompilationUnit(0), line, col);
        assertNotNull("Expected to find " + clazz + " at " + line + ":" + col + " but found nothing", pos);
        assertTrue("Expected " + clazz + " but found " + pos.getContextNode().getClass(),
                clazz.isInstance(pos.getContextNode()));
    }

}
