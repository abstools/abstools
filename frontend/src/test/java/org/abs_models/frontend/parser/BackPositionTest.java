/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.frontend.parser;

import static org.abs_models.ABSTest.Config.WITHOUT_MODULE_NAME;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import org.abs_models.frontend.FrontendTest;
import org.abs_models.frontend.ast.Block;
import org.abs_models.frontend.ast.ClassDecl;
import org.abs_models.frontend.ast.DeltaID;
import org.abs_models.frontend.ast.Deltaspec;
import org.abs_models.frontend.ast.FnApp;
import org.abs_models.frontend.ast.Model;
import org.abs_models.frontend.ast.ModuleDecl;
import org.abs_models.frontend.ast.StringLiteral;
import org.abs_models.frontend.ast.TypeUse;
import org.abs_models.frontend.ast.UnresolvedTypeUse;
import org.abs_models.frontend.ast.VarDecl;
import org.abs_models.frontend.ast.VarUse;
import org.junit.Test;

public class BackPositionTest extends FrontendTest {
    @Test
    public void test1() {
        assertNodeAtPos("module M; import * from ABS.StdLib;", 1, 1, ModuleDecl.class);
    }

    @Test
    public void test2() {
        assertNodeAtPos("module M; import * from ABS.StdLib;\nclass C {    }", 2, 14, ClassDecl.class);
    }

    @Test
    public void test3() {
        assertNodeAtPos("module M; import * from ABS.StdLib;\n     class C {}", 2, 4, ModuleDecl.class);
    }

    @Test
    public void test4() {
        assertNodeAtPos("module M; import * from ABS.StdLib;\nclass C { C m() {   } }", 2, 21, Block.class);
    }

    @Test
    public void test5() {
        assertNodeAtPos("module M; import * from ABS.StdLib;\ninterface Intf { } class C { C m() { Intf someName; } } ", 2, 43, VarDecl.class);
    }

    @Test
    public void test6() {
        // do not check for InterfaceTypeUse since if we skip type inference it might be UnresolvedTypeUse
        assertNodeAtPos("module M; import * from ABS.StdLib;\ninterface Intf { } class C { C m() { Intf someName; } } ", 2, 41, TypeUse.class);
    }


    @Test
    public void testString() {
        assertNodeAtPos("module M; import * from ABS.StdLib;\n  def String abc() = \"a\";\n" + 
                  "{ String s = \"abc\" + abc(); } ", 3, 15, StringLiteral.class);
    }

    @Test
    public void testString2() {
	assertNodeAtPos("module M; import * from ABS.StdLib;\n  def String abc() = \"a\";\n" +
			"{ String s = \"abc\" + abc(); } ", 3, 24, FnApp.class);
    }

    @Test
    public void testMainBlock() {
        assertNodeAtPos("module M; import * from ABS.StdLib;\ninterface I {} { I i; i = null;    }", 2, 36, Block.class);
    }

    @Test
    public void testListFunction() {
        assertNodeAtPos("module M; import * from ABS.StdLib;\n{ Int abc = 2; List<Int> ints = list[1, abc, 3]; }", 2, 40, VarUse.class);
    }

    @Test
    public void testEndLine() {
	/* If we don't import StdLib, it will be injected. Earlier versions of the frontend would make this
	   "virtual" statement have a bogus value for getEndLine() (end-of-file), which would trip up the bisection.
           We're here just reusing one of the test cases above, which one exactly is of no consequence. */
        assertNodeAtPos("module M; \n{ Int abc = 2; List<Int> ints = list[1, abc, 3]; }", 2, 40, VarUse.class);
    }

    @Test
    public void testFuncDef() {
        assertNodeAtPos("module M; import * from ABS.StdLib;\ndef Int foo(Int abc) = abc + 1;", 2, 23, VarUse.class);
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
        assertNodeAtPos("module Foo; import * from ABS.StdLib;\ntype InKeyType = String; type InValueType = List<String>; type OutKeyType = String; type OutValueType = Int; interface IMap {  List<Pair<OutKeyType, OutValueType>> invokeMap(InKeyType key, InValueType value);}"
                , 2, 161, UnresolvedTypeUse.class);
    }

    private void assertNodeAtPos(String absCode, int line, int col, Class<?> clazz) {
        Model m = assertParse(absCode, WITHOUT_MODULE_NAME);
        SourcePosition pos = SourcePosition.findPosition(m.getCompilationUnit(1), line, col);
        assertNotNull("Expected to find " + clazz + " at " + line + ":" + col + " but found nothing", pos);
        assertTrue("Expected " + clazz + " but found " + pos.getContextNode().getClass(),
                clazz.isInstance(pos.getContextNode()));
    }

}
