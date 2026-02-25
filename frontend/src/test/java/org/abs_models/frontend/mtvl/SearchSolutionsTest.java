/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.frontend.mtvl;

import static org.junit.Assert.*;

import java.util.Map;

import org.abs_models.common.WrongProgramArgumentException;
import org.junit.Test;

import org.abs_models.frontend.FrontendTest;
import org.abs_models.frontend.ast.Model;
import org.abs_models.frontend.ast.Product;
import org.abs_models.frontend.ast.ProductDecl;

public class SearchSolutionsTest extends FrontendTest {

    static private String helloprogram = """
        module Helloworld;
        product P1 (English);
        product P2 (French);
        product P3 (French, Repeat{times=10});
        product P4 (English, Repeat{times=6}); // wrong
        root MultiLingualHelloWorld {
          group allof {
             Language {
               group oneof { English, Dutch, French, German }
             },
             opt Repeat {
               Int times in [0 .. 10];
               ifin: times > 0;
             }
           }
        }
        extension English {
           ifin: Repeat ->
                 (Repeat.times >= 2 && Repeat.times <= 5);
        }
        """;

    @Test
    public void SearchSolutions() throws Exception {
        Model model = assertParse(helloprogram);
        model.setNullPrintStream();

        model.evaluateAllProductDeclarations();
        ChocoSolver s = ChocoSolver.fromModel(model);
        assertEquals(78,s.countSolutions());

        assertTrue(ChocoSolver.checkProduct(model.findProduct("P1").getProduct(), model).isEmpty());
        assertTrue(ChocoSolver.checkProduct(model.findProduct("P2").getProduct(),model).isEmpty());
        assertTrue(ChocoSolver.checkProduct(model.findProduct("P3").getProduct(),model).isEmpty());
        assertFalse(ChocoSolver.checkProduct(model.findProduct("P4").getProduct(),model).isEmpty());
    }

    @Test
    public void SearchSolutionsNoAttr() {
        Model model = assertParse(helloprogram);
        model.dropAttributes();

        ChocoSolver s = ChocoSolver.fromModel(model);

        assertEquals(8,s.countSolutions());
    }


    static private String withoutProducLine = "product P(); root FM";

    @Test
    public void CheckEmptyProduct() throws WrongProgramArgumentException {
        Model model = assertParse(withoutProducLine);

        model.evaluateAllProductDeclarations();

        ProductDecl product = model.findProduct("P");
        assertEquals(true, ChocoSolver.checkProduct(product.getProduct(), model).isEmpty());
    }
}
