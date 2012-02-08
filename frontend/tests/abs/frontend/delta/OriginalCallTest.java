/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.delta;

import org.junit.Test;
import static org.junit.Assert.*;

import java.util.ArrayList;
import java.util.Arrays;

import abs.frontend.ast.*;
import abs.frontend.delta.exceptions.*;

public class OriginalCallTest extends DeltaFlattenerTest {

    @Test
    public void originalCall() throws ASTNodeNotFoundException {
        Model model = assertParseOk(
                "module M;"
                + "interface I {}"
                + "class C implements I { Unit m() {} }"
                + "delta D { modifies class C { modifies Unit m() { original(); } } }"
                + "delta D2 { modifies class C { modifies Unit m() { original(); } } }"
        );
        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertTrue(cls.getMethods().getNumChild() == 1);
        
        DeltaDecl delta1 = (DeltaDecl) findDecl(model, "M", "D");
        assertTrue(delta1.getNumClassOrIfaceModifier() == 1);
        assertTrue(((ModifyClassModifier) delta1.getClassOrIfaceModifier(0)).getNumModifier() == 1);
        
        DeltaDecl delta2 = (DeltaDecl) findDecl(model, "M", "D2");
        assertTrue(delta2.getNumClassOrIfaceModifier() == 1);
        assertTrue(((ModifyClassModifier) delta2.getClassOrIfaceModifier(0)).getNumModifier() == 1);

        model.resolveOriginalCalls(new ArrayList<DeltaDecl>(Arrays.asList(delta1,delta2)));
        assertTrue(delta1.getNumClassOrIfaceModifier() == 2);
        assertTrue(delta2.getNumClassOrIfaceModifier() == 2);
        
        model.applyDeltas(new ArrayList<DeltaDecl>(Arrays.asList(delta1,delta2)));
        
        // there should be 3 methods now: the original one and those added by the two deltas
        assertEquals(3, cls.getMethods().getNumChild());
        assertTrue(cls.getMethod(0).getMethodSig().getName().equals("m"));
        assertTrue(cls.getMethod(1).getMethodSig().getName().equals("m$ORIGIN_core"));
        assertTrue(cls.getMethod(2).getMethodSig().getName().equals("m$ORIGIN_D"));
    }

    @Test
    public void oneDeltaMultipleCalls() throws ASTNodeNotFoundException {
        Model model = assertParseOk(
                "module M;"
                + "interface I {}"
                + "class C implements I { Unit m() {} Unit n() {} Unit p() {} }"
                + "delta D { modifies class C {"
                    + "modifies Unit m() { original(); }" 
                    + "modifies Unit n() { original(); }"
                + "} }"
        );

        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        DeltaDecl delta = (DeltaDecl) findDecl(model, "M", "D");
        assertEquals(1, delta.getNumClassOrIfaceModifier());

        model.resolveOriginalCalls(new ArrayList<DeltaDecl>(Arrays.asList(delta)));
        assertEquals(3, delta.getNumClassOrIfaceModifier());
    }

    @Test
    public void multipleCallsToSameMethod() throws ASTNodeNotFoundException {
        Model model = assertParseOk(
                "module M;"
                + "interface I {}"
                + "class C implements I { Unit m() {} }"
                + "delta D { modifies class C {"
                    + "modifies Unit m() { original(); original(); }" 
                + "} }"
        );

        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        DeltaDecl delta = (DeltaDecl) findDecl(model, "M", "D");
        assertEquals(1, delta.getNumClassOrIfaceModifier());

        model.resolveOriginalCalls(new ArrayList<DeltaDecl>(Arrays.asList(delta)));
        
        // TODO make sure the original method is copied only once, even with multiple original calls
        assertEquals(2, delta.getNumClassOrIfaceModifier());
    }
    
    
    @Test
    public void targetedOriginalCall() throws ASTNodeNotFoundException {
        Model model = assertParseOk(
                "module M;"
                + "class C {}"
                + "delta D1 { modifies class C { adds Unit m() {} } }"
                + "delta D2 { modifies class C { modifies Unit m() { original(); D1.original(); } } }"
        );
        
        DeltaDecl d1 = (DeltaDecl) findDecl(model, "M", "D1");
        DeltaDecl d2 = (DeltaDecl) findDecl(model, "M", "D2");
        model.resolveOriginalCalls(new ArrayList<DeltaDecl>(Arrays.asList(d1,d2)));
        model.applyDeltas(new ArrayList<DeltaDecl>(Arrays.asList(d1,d2)));
        
        // TODO make sure the original method is copied only once, even with multiple original calls
        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertEquals(cls.getMethods().getNumChild(), 2);
        assertTrue(cls.getMethod(0).getMethodSig().getName().equals("m"));
        assertTrue(cls.getMethod(1).getMethodSig().getName().equals("m$ORIGIN_D1"));
    }
}
