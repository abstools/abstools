/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.delta;

import static org.junit.Assert.assertTrue;

import org.junit.Test;

import abs.common.WrongProgramArgumentException;
import abs.frontend.ast.*;
import abs.frontend.delta.exceptions.ASTNodeNotFoundException;


public class DeltaAttributesTest extends DeltaFlattenerTest {

    @Test
    public void passFeatureAsBoolean() throws ASTNodeNotFoundException, WrongProgramArgumentException {
        Model model = assertParseOk(
                "module M; " 
                + "delta D(Boolean f) {adds class C { Boolean myField = f; } }" 
                + "productline PL { features F, G; delta D(F) when F; delta D(F) when G; } "
                + "product P1(F); "
                + "product P2(G);"
        );
        Model model2 = model.fullCopy();
        
        // flatten for P1 - f is true
        model.flattenForProduct("M.P1");
        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertTrue(cls.getField(0).getName().equals("myField"));
        assertTrue(cls.getField(0).getInitExp().value.toString().equals("True()"));

        // flatten for P2 - f is false
        model2.flattenForProduct("M.P2");
        cls = (ClassDecl) findDecl(model2, "M", "C");
        assertTrue(cls.getField(0).getName().equals("myField"));
        assertTrue(cls.getField(0).getInitExp().value.toString().equals("False()"));
    }

}
