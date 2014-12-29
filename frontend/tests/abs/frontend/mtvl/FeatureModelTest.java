/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.mtvl;

import org.junit.Test;

import abs.frontend.FrontendTest;
import abs.frontend.ast.Model;

public class FeatureModelTest extends FrontendTest {

    @Test
    public void minimalFM() {
        Model model = assertParseOk(
                "root FM"
                );
    }
    @Test
    public void minimalFMwithProduct() {
        Model model = assertParseOk(
                "product P (FM);"
                        + "root FM"
                );
    }

    @Test
    public void allValidProducts() {
        Model model = assertParseOk(
                "root Root {"
                        + " group allof {"
                        + "A,"
                        + "B,"
                        + "C { Int attrib1 in [0..4]; }"
                        + "}"
                        + "}"
                );

        model.debug = true;

        //        model.dropAttributes();

        System.out.println("=================================");
        ChocoSolver solver = model.getCSModel();

        for (String key : model.ints().keySet()) {
            System.out.println("*** Int " + model.ints().get(key));
        }
        for (String el : model.bools()) {
            System.out.println("*** Bool " + el);
        }
        for (String el : model.features()) {
            System.out.println("*** Feature " + el);
        }
        System.out.println("=================================");

        int i=1;
        while(solver.solveAgain()) {
            System.out.println("------ "+(i++)+"------");
            System.out.print(solver.resultToString());
        }


    }

}
