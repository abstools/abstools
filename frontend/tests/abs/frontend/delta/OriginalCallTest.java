/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.delta;

import static org.junit.Assert.*;

import org.junit.Test;
import abs.frontend.ast.*;
import abs.frontend.delta.exceptions.*;


public class OriginalCallTest extends DeltaFlattenerTest {

    @Test
    public void originalCall() throws ASTNodeNotFoundException {
        Model model = assertParseOk(
                "module M;"
                + "class C { Unit m() {} }"
                + "delta D { modifies class C { modifies Unit m() { original(); } } }"
        );
        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertTrue(cls.getMethods().getNumChild() == 1);
        
        DeltaDecl delta = (DeltaDecl) findDecl(model, "M", "D");
        model.applyDelta(delta);
        
        // FIXME - there should be two methods now??
        assertTrue(cls.getMethods().getNumChild() == 1);
        assertTrue(cls.getMethod(0).getMethodSig().getName().equals("m"));

    }


}
