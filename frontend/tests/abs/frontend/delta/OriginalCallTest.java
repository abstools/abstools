/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.delta;

import org.junit.Test;
import static org.hamcrest.CoreMatchers.containsString;
import static org.junit.Assert.*;

import java.util.ArrayList;
import java.util.Arrays;

import abs.frontend.ast.*;

public class OriginalCallTest extends DeltaTest {

    @Test
    public void originalCall() throws DeltaModellingException {
        Model model = assertParseOk(
                "module M;"
                + "interface I {}"
                + "class C implements I { Unit m() {} }"
                + "delta D; uses M;"
                + "modifies class C { modifies Unit m() { original(); } }"
                + "delta D2; uses M;"
                + "modifies class C { modifies Unit m() { original(); } }"
        );
        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertTrue(cls.getMethods().getNumChild() == 1);

        DeltaDecl delta1 = findDelta(model, "D");
        assertTrue(delta1.getNumModuleModifier() == 1);
        assertTrue(((ModifyClassModifier) delta1.getModuleModifier(0)).getNumModifier() == 1);

        DeltaDecl delta2 = findDelta(model, "D2");
        assertTrue(delta2.getNumModuleModifier() == 1);
        assertTrue(((ModifyClassModifier) delta2.getModuleModifier(0)).getNumModifier() == 1);

        Model.resolveOriginalCalls(new ArrayList<DeltaDecl>(Arrays.asList(delta1,delta2)));
        assertTrue(delta1.getNumModuleModifier() == 2);
        assertTrue(delta2.getNumModuleModifier() == 2);

        model.applyDeltas(new ArrayList<DeltaDecl>(Arrays.asList(delta1,delta2)));

        // there should be 3 methods now: the original one and those added by the two deltas
        assertEquals(3, cls.getMethods().getNumChild());
        assertTrue(cls.getMethod(0).getMethodSig().getName().equals("m"));
        assertTrue(cls.getMethod(1).getMethodSig().getName().equals("m$ORIGIN_core"));
        assertTrue(cls.getMethod(2).getMethodSig().getName().equals("m$ORIGIN_D"));
    }


    @Test
    public void originalCall2() throws DeltaModellingException {
        Model model = assertParseOk(
                "module M;"
                + "interface I {}"
                + "class C implements I { Unit m() {} }"
                + "delta D; uses M;"
                + "modifies class C { modifies Unit m() { original(); } }"
                + "delta D2; uses M;"
                + "modifies class C { modifies Unit m() { original(); } }"
        );
        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertTrue(cls.getMethods().getNumChild() == 1);

        DeltaDecl delta1 = findDelta(model, "D");
        assertTrue(delta1.getNumModuleModifier() == 1);
        assertTrue(((ModifyClassModifier) delta1.getModuleModifier(0)).getNumModifier() == 1);

        DeltaDecl delta2 = findDelta(model, "D2");
        assertTrue(delta2.getNumModuleModifier() == 1);
        assertTrue(((ModifyClassModifier) delta2.getModuleModifier(0)).getNumModifier() == 1);

        Model.resolveOriginalCalls(new ArrayList<DeltaDecl>(Arrays.asList(delta1,delta2)));
        assertTrue(delta1.getNumModuleModifier() == 2);
        assertTrue(delta2.getNumModuleModifier() == 2);

        model.applyDeltas(new ArrayList<DeltaDecl>(Arrays.asList(delta1,delta2)));

        // there should be 3 methods now: the original one and those added by the two deltas
        assertEquals(3, cls.getMethods().getNumChild());
        assertTrue(cls.getMethod(0).getMethodSig().getName().equals("m"));
        assertTrue(cls.getMethod(1).getMethodSig().getName().equals("m$ORIGIN_core"));
        assertTrue(cls.getMethod(2).getMethodSig().getName().equals("m$ORIGIN_D"));
    }

    @Test
    public void originalCall3() throws DeltaModellingException {
        Model model = assertParseOk(
                "module M;"
                + "interface I {}"
                + "class C implements I { Int one() { return 1; } }"
                + "delta D; uses M;"
                + "modifies class C { modifies Int one() { Int x = original(); return x + 1; } }"
        );

        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertEquals(1, cls.getMethods().getNumChild());

        DeltaDecl delta1 = findDelta(model, "D");
        Model.resolveOriginalCalls(new ArrayList<DeltaDecl>(Arrays.asList(delta1)));
        model.applyDeltas(new ArrayList<DeltaDecl>(Arrays.asList(delta1)));

        assertEquals(2, cls.getMethods().getNumChild());
        assertTrue(cls.getMethod(0).getMethodSig().getName().equals("one"));
        // make sure method has the right body
        assertTrue(cls.getMethod(0).getBlock().getStmt(1) instanceof ReturnStmt);

        assertTrue(cls.getMethod(1).getMethodSig().getName().equals("one$ORIGIN_core"));
        // make sure method has the right body
        assertTrue(cls.getMethod(1).getBlock().getStmt(0) instanceof ReturnStmt);
    }

    @Test
    public void oneDeltaMultipleCalls() throws DeltaModellingException {
        Model model = assertParseOk(
                "module M;"
                + "interface I {}"
                + "class C implements I { Unit m() {} Unit n() {} Unit p() {} }"
                + "delta D;uses M;"
                + "modifies class C {"
                    + "modifies Unit m() { original(); }"
                    + "modifies Unit n() { original(); }"
                    + "modifies Unit p() { original(); }"
                + "}"
        );

        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        DeltaDecl delta = findDelta(model, "D");
        assertEquals(1, delta.getNumModuleModifier());

        Model.resolveOriginalCalls(new ArrayList<DeltaDecl>(Arrays.asList(delta)));
        assertEquals(4, delta.getNumModuleModifier());
    }

    @Test
    public void multipleCallsToSameMethod() throws DeltaModellingException {
        Model model = assertParseOk(
                "module M;"
                + "interface I {}"
                + "class C implements I { Unit m() {} }"
                + "delta D;"
                + "modifies class C {"
                    + "modifies Unit m() { original(); original(); }"
                + "}"
        );

        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        DeltaDecl delta = findDelta(model, "D");
        assertEquals(1, delta.getNumModuleModifier());

        Model.resolveOriginalCalls(new ArrayList<DeltaDecl>(Arrays.asList(delta)));

        assertEquals(delta.getModuleModifiers().toString(),2, delta.getNumModuleModifier());
    }

    @Test
    public void targetedAndUntargetedOriginalCall() throws DeltaModellingException {
        Model model = assertParseOk(
                "module M;"
                + "class C { Unit m() {} }"

                + "delta D1; "
                + "uses M;"
                + "modifies class C { adds Unit n() {} }"

                + "delta D2; "
                + "uses M;"
                + "modifies class C { modifies Unit m() { original(); core.original(); } }"

                + "delta D3; "
                + "uses M;"
                + "modifies class C { modifies Unit n() { original(); D1.original(); } }"
        );

        DeltaDecl d1 = findDelta(model, "D1");
        DeltaDecl d2 = findDelta(model, "D2");
        DeltaDecl d3 = findDelta(model, "D3");
        Model.resolveOriginalCalls(new ArrayList<DeltaDecl>(Arrays.asList(d1,d2,d3)));
        model.applyDeltas(new ArrayList<DeltaDecl>(Arrays.asList(d1,d2,d3)));

        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertEquals(cls.getMethods().toString(), 4, cls.getMethods().getNumChild());
        assertTrue(cls.getMethod(0).getMethodSig().getName().equals("m"));
        assertTrue(cls.getMethod(1).getMethodSig().getName().equals("n"));
        assertTrue(cls.getMethod(2).getMethodSig().getName().equals("m$ORIGIN_core"));
        assertTrue(cls.getMethod(3).getMethodSig().getName().equals("n$ORIGIN_D1"));
    }

    @Test
    public void targetedOriginalCall() throws DeltaModellingException {
        Model model = assertParseOk(
                "module M;"
                + "class C { Unit m() {} }"
                + "delta D1;"
                + "uses M;"
                + "modifies class C { modifies Unit m() { core.original(); } }"
                + "adds class C2 { }"
                + "delta D2;"
                + "modifies class M.C { modifies Unit m() { D1.original(); } }"
        );

        DeltaDecl d1 = findDelta(model, "D1");
        DeltaDecl d2 = findDelta(model, "D2");

        Model.resolveOriginalCalls(new ArrayList<DeltaDecl>(Arrays.asList(d1,d2)));
        model.applyDeltas(new ArrayList<DeltaDecl>(Arrays.asList(d1,d2)));

        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertEquals(cls.getMethods().toString(), 3, cls.getMethods().getNumChild());
        assertTrue(cls.getMethod(0).getMethodSig().getName().equals("m"));
        assertTrue(cls.getMethod(1).getMethodSig().getName().equals("m$ORIGIN_core"));
        assertTrue(cls.getMethod(2).getMethodSig().getName().equals("m$ORIGIN_D1"));
    }


    @Test
    public void multipleTargetedOriginalCalls() throws DeltaModellingException {
        Model model = assertParseOk(
                "module M;"
                + "class C { }"
                + "delta D1;"
                + "uses M;"
                + "modifies class C { adds Unit m() {} }"
                + "delta D2;uses M;"
                + "modifies class C { modifies Unit m() { D1.original(); } }"
                + "delta D3;uses M;"
                + "modifies class C { modifies Unit m() { D1.original(); } }"
        );

        DeltaDecl d1 = findDelta(model, "D1");
        DeltaDecl d2 = findDelta(model, "D2");
        DeltaDecl d3 = findDelta(model, "D3");
        Model.resolveOriginalCalls(new ArrayList<DeltaDecl>(Arrays.asList(d1,d2,d3)));
        model.applyDeltas(new ArrayList<DeltaDecl>(Arrays.asList(d1,d2,d3)));

        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertEquals(cls.getMethods().toString(), 2, cls.getMethods().getNumChild());
        assertTrue(cls.getMethod(0).getMethodSig().getName().equals("m"));
        assertTrue(cls.getMethod(1).getMethodSig().getName().equals("m$ORIGIN_D1"));
    }

    @Test(expected=DeltaModellingException.class)
    public void originalNotFound() throws DeltaModellingException {
        Model model = assertParseOk(
                "module M;"
                + "class C { }"
                + "delta D1;"

                + "modifies class C { modifies Unit m() { original(); } }"
        );

        DeltaDecl d1 = findDelta(model, "D1");

        Model.resolveOriginalCalls(new ArrayList<DeltaDecl>(Arrays.asList(d1)));
    }

    @Test(expected=DeltaModellingException.class)
    public void targetedOriginalNotFound() throws DeltaModellingException {
        Model model = assertParseOk(
                "module M;"
                + "class C { }"
                + "delta D1;"
                + "uses M;"
                + "modifies class C { modifies Unit m() { core.original(); } }"
        );

        DeltaDecl d1 = findDelta(model, "D1");

        Model.resolveOriginalCalls(new ArrayList<DeltaDecl>(Arrays.asList(d1)));
    }

    @Test
    public void targetedDeltaNotYetApplied() throws DeltaModellingException {
        Model model = assertParseOk(
                "module M;"
                + "class C { Unit m() {} }"
                + "delta D1;"
                + "uses M;"
                + "modifies class C { modifies Unit m() { core.original(); } }"
                + "delta D2;"
                + "uses M;"
                + "modifies class C { modifies Unit m() { D1.original(); } }"
        );

        DeltaDecl d1 = findDelta(model, "D1");
        DeltaDecl d2 = findDelta(model, "D2");
        try {
            // applying deltas in wrong order (D2 then D1) should throw an exception
            // because D2 has a targeted original call to a method in D1
            Model.resolveOriginalCalls(new ArrayList<DeltaDecl>(Arrays.asList(d2,d1)));
            fail("expected a DeltaModellingException");
        } catch (DeltaModellingException e) {
            assertThat(e.getMessage(), containsString("has not yet been applied"));
        }
    }


}
