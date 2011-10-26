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

public class DeltaAttributesMixedTest extends DeltaFlattenerTest {

    @Test
    public void passFeaturesAsBooleans() throws ASTNodeNotFoundException, WrongProgramArgumentException {
        Model model = assertParseOk(
                "module M;"
                + "delta D(Bool a, Bool b, Bool c, Int c_a1) { adds class C { "
                + "Bool fA = a; "
                + "Bool fB = b; "
                + "Bool fC = c; "
                + "Int fC_a1 = c_a1; "
                + "} }" 
                + "productline PL { features A,B,C; delta D(A,B,C) when C; }"
                + "product P1( C{a1=99} );"
        );
        
        model.flattenForProduct("M.P1");
        ClassDecl cls = (ClassDecl) findDecl(model, "M", "C");
        assertTrue(cls.getField(0).getName().equals("fA"));
        assertTrue(cls.getField(0).getInitExp().value.toString().equals("False()"));
        assertTrue(cls.getField(1).getName().equals("fB"));
        assertTrue(cls.getField(1).getInitExp().value.toString().equals("False()"));
        assertTrue(cls.getField(2).getName().equals("fC"));
        assertTrue(cls.getField(2).getInitExp().value.toString().equals("True()"));
        assertTrue(cls.getField(3).getName().equals("fC_a1"));
//        System.out.println("*** found: " + cls.getField(3).getInitExp().value.toString());
        assertTrue(cls.getField(3).getInitExp().value.toString().equals("IntLiteral(999)"));
    }


}
