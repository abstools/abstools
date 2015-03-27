/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.delta;

import static org.junit.Assert.*;

import java.util.List;
import java.util.Set;

import org.junit.Test;

import abs.common.WrongProgramArgumentException;
import abs.frontend.analyser.SemanticErrorList;
import abs.frontend.ast.Model;
import abs.frontend.ast.Product;
import abs.frontend.ast.ProductLine;

public class DeltaOrderingTest extends DeltaTest {

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

    @Test
    public void properSorting1() throws DeltaModellingException, WrongProgramArgumentException {
        Model model = assertParseOk(
                "module Test;"
                        + "delta D1;"
                        + "delta D2;"
                        + "productline PL;"
                        + "features A,B;"
                        + "delta D1 when A;"
                        + "delta D2 after D1 when B;"
                        + "product P1(A);"
                        + "product P2(A, B);"
                );

        Product prod = model.findProduct("P1");
        ProductLine pl = model.getProductLine();
        Set<String> deltaids = pl.findApplicableDeltas(prod);
        List<String> sorted_deltaids = pl.sortDeltas(deltaids);
        assertArrayEquals(new String[]{ "D1" }, sorted_deltaids.toArray());

        prod = model.findProduct("P2");
        deltaids = pl.findApplicableDeltas(prod);
        sorted_deltaids = pl.sortDeltas(deltaids);
        assertArrayEquals(new String[]{ "D1", "D2" }, sorted_deltaids.toArray());
    }

    @Test
    public void properSorting9() throws DeltaModellingException, WrongProgramArgumentException {
        Model model = assertParseOk(
                "module Test;"
                        + "delta D1;"
                        + "delta D2;"
                        + "delta D3;"
                        + "delta D4;"
                        + "delta D5;"
                        + "delta D6;"
                        + "delta D7;"
                        + "delta D8;"
                        + "delta D9;"
                        + "productline PL;"
                        + "features A,B,C,D,E,F,G,H,I;"
                        + "delta D1 after D2 when A;"
                        + "delta D2 after D3 when B;"
                        + "delta D3 after D4 when C;"
                        + "delta D4 after D5 when D;"
                        + "delta D5 after D6 when E;"
                        + "delta D6 after D7 when F;"
                        + "delta D7 after D8 when G;"
                        + "delta D8 after D9 when H;"
                        + "delta D9 when I;"
                        + "product P1(A,B,C,D,E,F,G,H,I);"
                        + "product P2(A,C,E,G,I);"
                );

        Product prod = model.findProduct("P1");
        ProductLine pl = model.getProductLine();
        Set<String> deltaids = pl.findApplicableDeltas(prod);
        List<String> sorted_deltaids = pl.sortDeltas(deltaids);
        assertArrayEquals(new String[]{"D9", "D8", "D7", "D6", "D5", "D4", "D3", "D2", "D1" }, sorted_deltaids.toArray());

        prod = model.findProduct("P2");
        deltaids = pl.findApplicableDeltas(prod);
        sorted_deltaids = pl.sortDeltas(deltaids);
        assertArrayEquals(new String[]{ "D9", "D7", "D5", "D3", "D1" }, sorted_deltaids.toArray());

    }
}
