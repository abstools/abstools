/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.mtvl;

import static org.junit.Assert.*;

import java.util.Map;

import org.junit.Test;

import abs.common.WrongProgramArgumentException;
import abs.frontend.FrontendTest;
import abs.frontend.ast.Model;
import abs.frontend.ast.Product;
import abs.frontend.ast.ProductDecl;

public class SearchSolutionsTest extends FrontendTest {

    static private boolean checkSol(ChocoSolver s, Model m, String prod) throws Exception {
        Product p = m.findProduct(prod).getProduct();
        assertNotNull(p);
        Map<String,Integer> guess = p.getSolution();
        return s.checkSolution(guess,m);
    }

    static private String helloprogram =
        " module Helloworld;" +
        " product P1 (English);" +
        " product P2 (French);" +
        " product P3 (French, Repeat{times=10});" +
        " product P4 (English, Repeat{times=6});" +
        " root MultiLingualHelloWorld {" +
        "   group allof {" +
        "      Language {" +
        "        group oneof { English, Dutch, French, German }" +
        "      }," +
        "      opt Repeat {" +
        "        Int times in [0..10];" +
        "        ifin: times > 0; " +
        "      } " +
        "    } " +
        " }" +
        " extension English {" +
        "    ifin: Repeat ->" +
        "          (Repeat.times >= 2 && Repeat.times <= 5);" +
        " }";


    @Test
    public void SearchSolutions() throws Exception {
        Model model = assertParseOk(helloprogram);
        model.setNullPrintStream();

        ChocoSolver s = model.instantiateCSModel();
        model.evaluateAllProductDeclarations();

        assertEquals(78,s.countSolutions());
        assertTrue(checkSol(s,model,"P1"));
        assertTrue(checkSol(s,model,"P2"));
        assertTrue(checkSol(s,model,"P3"));
        assertTrue(!checkSol(s,model,"P4"));
    }

    @Test
    public void SearchSolutionsNoAttr() {
        Model model = assertParseOk(helloprogram);
        model.dropAttributes();

        ChocoSolver s = model.instantiateCSModel();

        assertEquals(8,s.countSolutions());
    }


    static private String withoutProducLine = "product P(); root FM";

    @Test
    public void CheckEmptyProduct() throws WrongProgramArgumentException {
        Model model = assertParseOk(withoutProducLine);

        ChocoSolver s = model.instantiateCSModel();
        model.evaluateAllProductDeclarations();

        ProductDecl product = model.findProduct("P");
        Map<String,Integer> guess = product.getProduct().getSolution();
        assertEquals(true, s.checkSolution(guess,model));
    }
}
