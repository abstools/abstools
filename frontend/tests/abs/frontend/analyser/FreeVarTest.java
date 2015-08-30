/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.analyser;

import static org.junit.Assert.*;

import java.util.*;

import org.junit.Assert;
import org.junit.Test;

import abs.frontend.FrontendTest;
import abs.frontend.ast.Exp;

public class FreeVarTest extends FrontendTest {

    @Test
    public void dataConstrExp() {
        Exp e = getFirstExp("{ Bool b = True; }");
        assertTrue(e.getFreeVars().isEmpty());
    }

    @Test
    public void varUseExp() {
        Exp e = getSecondExp("{ Bool b = True; Bool c = b; }");
        assertEquals(e.getFreeVars(), "b");
    }

    @Test
    public void letExp() {
        Exp e = getSecondExp("{ Bool b = True; Bool c = let (Bool d) = True in b; }");
        assertEquals(e.getFreeVars(), "b");
    }

    @Test
    public void letExp2() {
        Exp e = getSecondExp("{ Bool b = True; Bool c = let (Bool d) = b in d; }");
        assertEquals(e.getFreeVars(), "b");
    }

    @Test
    public void letExp3() {
        Exp e = getSecondExp("{ Bool b = True; Bool c = let (Bool b) = b in b; }");
        assertEquals(e.getFreeVars(), "b");
    }

    @Test
    public void letExp4() {
        Exp e = getSecondExp("{ Bool b = True; Bool c = let (Bool b) = True in b; }");
        assertTrue(e.getFreeVars().isEmpty());
    }

    @Test
    public void caseExp() {
        Exp e = getSecondExp("{ Bool b = True; Bool c = case True { y => y; }; }");
        assertTrue(e.getFreeVars().isEmpty());
    }

    @Test
    public void unaryExp() {
        Exp e = getSecondExp("{ Bool b = True; Bool c = !b; }");
        assertEquals(e.getFreeVars(), "b");
    }

    @Test
    public void binaryExp() {
        Exp e = getSecondExp("{ Bool b = True; Bool c = b == True; }");
        assertEquals(e.getFreeVars(), "b");
    }

    @Test
    public void binaryExp2() {
        Exp e = getSecondExp("{ Bool b = True; Bool c = True != b; }");
        assertEquals(e.getFreeVars(), "b");
    }

    @Test
    public void binaryExp3() {
        Exp e = getSecondExp("{ Bool b = True; Bool c = b == b; }");
        assertEquals(e.getFreeVars(), "b");
    }

    @Test
    public void callExp() {
        Exp e = getExp("interface I { Unit m(Bool b); } { Bool b = True; I i; Bool c = i.m(b); }", 2);
        assertEquals(e.getFreeVars(), "i", "b");
    }

    @Test
    public void callExp2() {
        Exp e = getExp("interface I { Unit m(Bool b); } { Bool b = True; I i; Bool c = i.m(True); }", 2);
        assertEquals(e.getFreeVars(), "i");
    }

    @Test
    public void asyncCallExp() {
        Exp e = getExp("interface I { Unit m(Bool b); } { Bool b = True; I i; Bool c = i!m(b); }", 2);
        assertEquals(e.getFreeVars(), "i", "b");
    }

    @Test
    public void newExp() {
        Exp e = getSecondExp("class C(Bool b) {}  { Bool b = True; new local C(b); }");
        assertEquals(e.getFreeVars(), "b");
    }

    @Test
    public void getExp() {
        Exp e = getSecondExp("{ Fut<Bool> f; f.get; }");
        assertEquals(e.getFreeVars(), "f");
    }

    @Test
    public void fnApp() {
        Exp e = getSecondExp("def Unit f(Bool b) = Unit; { Bool b; Unit u = f(b); }");
        assertEquals(e.getFreeVars(), "b");
    }

    @Test
    public void constructorApp() {
        Exp e = getSecondExp("data D = C(Bool);{ Bool b; D d = C(b); }");
        assertEquals(e.getFreeVars(), "b");
    }

    @Test
    public void ifExpr() {
        Exp e = getExp("{ Int x = 3; Int y = 4; Int z = 5; Int a = if x == 3 then y else z; }", 3);
        assertEquals(e.getFreeVars(), "x", "y", "z");
    }
    
    public void assertEquals(Set<String> actual, String... expected) {
        Assert.assertEquals(new HashSet<String>(Arrays.asList(expected)), actual);
    }
}
