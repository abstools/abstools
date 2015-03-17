/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.delta;

import static org.junit.Assert.*;

import org.junit.Test;

import abs.frontend.analyser.SemanticErrorList;
import abs.frontend.ast.Model;
import abs.frontend.ast.ProductLine;

public class DeltaTrieTest extends DeltaTest {

    @Test
    public void test1() {

        Model model = assertParseOk(
                "productline PL;"
                        + "features A,B,C;"
                        + "delta D1 when A;"
                        + "delta D2 after D1 when B;"
                        + "delta D3 after D2 when C;"
                        + "root FM {"
                        + " group [1..*] { A, B, C }"
                        + "}"
                        + "extension B { require: A; }"
                        + "extension C { require: B; }"
                );

        ProductLine pl = model.getProductLine();
        TopologicalSorting<String> sorter = new TopologicalSorting<String>(pl.getAllDeltaIDs());
        DeltaTrie pfgt = ProductLineTypeAnalysisHelper.buildPFGT(pl, sorter, new SemanticErrorList());

        // TODO: tests
    }

}
