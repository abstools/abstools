/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.delta;

import static org.junit.Assert.*;

import org.junit.Test;

import abs.frontend.analyser.SemanticConditionList;
import abs.frontend.ast.Model;
import abs.frontend.ast.ProductLine;

public class DeltaTrieTest extends DeltaTest {

    @Test
    public void trieStructure1() {
        Model model = assertParseOk(
                "module Test;"
                        + "delta D1;"
                        + "delta D2;"
                        + "productline PL;"
                        + "features A,B;"
                        + "delta D1 after D2 when A;"
                        + "delta D2 when B;"
                        + "root FM {"
                        + " group [0..*] { A, B }"
                        + "}"
                );
        ProductLine pl = model.getProductLine();
        DeltaTrie pfgt = ProductLineTypeAnalysisHelper.buildPFGT(pl, new SemanticConditionList());

        assertTrue(pfgt.getRoot().isValidProduct());
        assertEquals(2, pfgt.getRoot().getChildren().size());
        assertTrue(pfgt.getRoot().getChildren().get("D1").isValidProduct());
        assertTrue(pfgt.getRoot().getChildren().get("D2").isValidProduct());
        assertEquals(0, pfgt.getRoot().getChildren().get("D1").getChildren().size());
        assertEquals(1, pfgt.getRoot().getChildren().get("D2").getChildren().size());
        assertTrue(pfgt.getRoot().getChildren().get("D2").getChildren().get("D1").isValidProduct());
    }

    @Test
    public void trieStructure2() {
        Model model = assertParseOk(
                "module Test;"
                        + "delta D1;"
                        + "delta D2;"
                        + "delta D3;"
                        + "productline PL;"
                        + "features A,B,C;"
                        + "delta D1 when A;"
                        + "delta D2 after D1 when B;"
                        + "delta D3 after D2 when C;"
                        + "root FM {"
                        + " group [0..*] { A, B, C }"
                        + "}"
                );
        ProductLine pl = model.getProductLine();
        DeltaTrie pfgt = ProductLineTypeAnalysisHelper.buildPFGT(pl, new SemanticConditionList());

        assertTrue(pfgt.getRoot().isValidProduct());
        assertEquals(3, pfgt.getRoot().getChildren().size());
        assertTrue(pfgt.getRoot().getChildren().get("D1").isValidProduct());
        assertTrue(pfgt.getRoot().getChildren().get("D2").isValidProduct());
        assertTrue(pfgt.getRoot().getChildren().get("D3").isValidProduct());
        assertEquals(2, pfgt.getRoot().getChildren().get("D1").getChildren().size());
        assertEquals(1, pfgt.getRoot().getChildren().get("D2").getChildren().size());
        assertEquals(0, pfgt.getRoot().getChildren().get("D3").getChildren().size());
        assertTrue(pfgt.getRoot().getChildren().get("D1").getChildren().get("D2").isValidProduct());
        assertTrue(pfgt.getRoot().getChildren().get("D1").getChildren().get("D3").isValidProduct());
        assertTrue(pfgt.getRoot().getChildren().get("D2").getChildren().get("D3").isValidProduct());
    }


    @Test
    public void trieContent() {
        // TODO
        Model model = assertParseOk(
                "module Test;"
                        + "class Ccore{}"
                        + "delta D1; uses Test; adds class C1 {Int f1 = 0; Unit m1() {}}"
                        + "delta D2; uses Test; adds class C2 {Int f2 = 0; Unit m2() {}}"
                        + "delta D3;"
//                        + " modifies class Test.C1 { removes Int f1; removes Unit m1(); }"
//                        + " modifies class Test.C2 { adds Int f2a = 1; modifies Unit m2() {} adds Unit m2a() {} }"
                        + ""
                        + ""
                        + ""
                        + ""
                        + "productline PL;"
                        + "features A,B;"
                        + "delta D1 when A;"
                        + "delta D2 after D1 when B;"
                        + "delta D3 after D1,D2 when A && B;"
                        + "root FM {"
                        + " group [1..*] { A, B }"
                        + "}"
                );

        ProductLine pl = model.getProductLine();
        DeltaTrie pfgt = ProductLineTypeAnalysisHelper.buildPFGT(pl, new SemanticConditionList());

        // TODO: tests
    }

}
