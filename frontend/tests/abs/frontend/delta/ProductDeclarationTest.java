/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.delta;

import static org.junit.Assert.*;

import org.junit.Test;

import abs.frontend.FrontendTest;
import abs.frontend.ast.*;

public class ProductDeclarationTest extends FrontendTest{

    @Test
    public void productDeclStructure() {
        Model model = assertParseOk(
                "product P1(F1, F2, F3);"
                + "product P2(F3, F4);"
                + "product P3(F5);"
                + "product P4 = P1 && P2; // union"
                + "product P5 = P1 || P2; // intersection"
                + "product P6 = P1 && P2 || P3 || {F7, F8}; // complex expression"
                + "product P7 = P1 && (P2 || P3) || {F7, F8}; // complex expression with parenthesis"
                );
    }
    
    @Test
    public void productDeclCyclic() {
        Model model = assertParseOk(
                "product P1(F1, F2, F3);"
                + "product P2(F3, F4);"
                + "product P4 = P1 && P2 || P5;"
                + "product P5 = P1 || P2 || P4;"
                );
    }
    
    @Test
    public void productDeclNotFound() {
        Model model = assertParseOk(
                "product P1(F1, F2, F3);"
                + "product P2(F3, F4);"
                + "product P3(F5);"
                + "product P4 = P1 && P2 || P5;"
                );
    }
}
