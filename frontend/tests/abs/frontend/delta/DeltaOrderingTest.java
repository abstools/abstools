/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.delta;

import static org.junit.Assert.*;

import org.junit.Test;

import abs.common.WrongProgramArgumentException;
import abs.frontend.ast.Model;

public class DeltaOrderingTest extends DeltaTest {

    @Test
    public void incompleteListOfDeltas() throws DeltaModellingException, WrongProgramArgumentException {
        Model model = assertParseOk(
                "module M;"
                        + "delta D1;"
                        + "delta D2;"
                        + "productline PL;"
                        + "features A;"
                        + "delta D1 after D2 when A;"
                        + "product P(A);"

                );
        model.flattenForProduct("P");
        
    }

    
    @Test(expected=DeltaModellingException.class)
    public void circularOrder1() throws DeltaModellingException, WrongProgramArgumentException {
        Model model = assertParseOk(
                "module M;"
                        + "delta D1;"
                        + "delta D2;"
                        + "productline PL;"
                        + "features A;"
                        + "delta D1 after D2 when A;"
                        + "delta D2 after D1 when A;"
                        + "product P(A);"

                );
        model.flattenForProduct("P");

    }

    @Test(expected=DeltaModellingException.class)
    public void circularOrder2() throws DeltaModellingException, WrongProgramArgumentException {
        Model model = assertParseOk(
                "module M;"
                        + "delta D1;"
                        + "delta D2;"
                        + "delta D3;"
                        + "delta D4;"
                        + "productline PL;"
                        + "features A;"
                        + "delta D1 after D2 when A;"
                        + "delta D2 after D1 when A;"
                        + "delta D3 after D1 when A;"
                        + "delta D1 after D4 when A;"
                        + "product P(A);"

                );
        model.flattenForProduct("P");

    }

}
