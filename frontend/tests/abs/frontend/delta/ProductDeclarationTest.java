/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.delta;

import static org.junit.Assert.*;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;

import org.junit.Test;

import abs.common.WrongProgramArgumentException;
import abs.frontend.FrontendTest;
import abs.frontend.ast.*;

public class ProductDeclarationTest extends FrontendTest{

    /*
    @Test
    public void productExprResult() {
        Model model = assertParseOk(
                "product P1(F1, F2, F3);"
                        + "product P2(F3, F4);"
                        + "product P3(F5);"
                        + "product P4 = P1 && P2; // union"
                        + "product P5 = P1 || P2; // intersection"
                        + "product P6 = P1 && P2 || P3 || {F7, F8} || P5; // complex expression"
                        + "product P7 = P1 && (P2 || P3) || {F7, F8} || P5; // complex expression with parenthesis"
                );

        HashMap<String, List<String>> products = new HashMap<String, List<String>>();
        products.put("P1", Arrays.asList("F1", "F2", "F3"));
        products.put("P2", Arrays.asList("F3", "F4"));
        products.put("P3", Arrays.asList("F5"));
        products.put("P4", Arrays.asList("F3"));
        products.put("P5", Arrays.asList("F1", "F2", "F3", "F4"));
        products.put("P6", Arrays.asList("F1", "F2", "F3", "F4", "F5", "F7", "F8"));
        products.put("P7", Arrays.asList("F1", "F2", "F3", "F4", "F7", "F8"));

        for (CompilationUnit u : model.getCompilationUnits()) {
            for (ProductDecl p : u.getProductDecls()) {
                List<String> features = products.get(p.getName());

                for(Feature f : p.getImplicitProduct().getFeatures()){
                    assertTrue(features.contains(f.getName()));
                }
            }
        }
    }

    @Test(expected=DeltaModellingException.class)
    public void productDeclCyclic() {
        Model model = assertParseOk(
                "product P4 = P5;"
                        + "product P5 = P4;"
                );
    }

    @Test(expected=WrongProgramArgumentException.class)
    public void productDeclNotFound() {
        Model model = assertParseOk("product P1 = P2;");
    }
    */
}
