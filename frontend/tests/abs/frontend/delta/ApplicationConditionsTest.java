/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.delta;

import org.junit.Ignore;
import org.junit.Test;

import abs.frontend.ast.Model;
import static org.junit.Assert.*;
import abs.frontend.ast.*;


public class ApplicationConditionsTest extends DeltaTest {

    @Test
    public void singleAppCondFeature() {
        Model model = assertParseOk(
                "productline PL;"
                        + "features A,B,C,D;"
                        + "delta D when A;"
                );
        AppCond a = model.getProductLine().getDeltaClause(0).getAppCond();
        assertEquals(((AppCondFeature)a).getName(), "A");
    }

    @Test
    public void conjunction() {
        Model model = assertParseOk(
                "productline PL;"
                        + "features A,B,C,D;"
                        + "delta D when A && B;"
                );
        AppCond a = model.getProductLine().getDeltaClause(0).getAppCond();
        assertEquals(((AppCondFeature)((AppCondAnd)a).getLeft()).getName(), "A");
        assertEquals(((AppCondFeature)((AppCondAnd)a).getRight()).getName(), "B");
    }

    @Test
    public void disjunction() {
        Model model = assertParseOk(
                "productline PL;"
                        + "features A,B,C,D;"
                        + "delta D when A || B;"
                );
        AppCond a = model.getProductLine().getDeltaClause(0).getAppCond();
        assertEquals(((AppCondFeature)((AppCondOr)a).getLeft()).getName(), "A");
        assertEquals(((AppCondFeature)((AppCondOr)a).getRight()).getName(), "B");
    }

    @Test
    public void negation() {
        Model model = assertParseOk(
                "productline PL;"
                        + "features A,B,C,D;"
                        + "delta D when ~A;"
                );
        AppCond a = model.getProductLine().getDeltaClause(0).getAppCond();
        assertEquals(((AppCondFeature)((AppCondNot)a).getAppCond()).getName(), "A");
    }

    @Test
    public void parenthesis() {
        Model model = assertParseOk(
                "productline PL;"
                        + "features A,B,C,D;"
                        + "delta D when (A);"
                );
        AppCond a = model.getProductLine().getDeltaClause(0).getAppCond();
        assertEquals(((AppCondFeature)a).getName(), "A");
    }

    @Test
    public void complexExpression1() {
        Model model = assertParseOk(
                "productline PL;"
                        + "features A,B,C,D;"
                        + "delta D when (A || B) && ~(C || D);"
                );
        AppCond a = model.getProductLine().getDeltaClause(0).getAppCond();
        assertEquals(((AppCondFeature)((AppCondOr)(((AppCondAnd)a).getLeft())).getLeft()).getName(), "A");
    }

    @Ignore
    @Test
    public void withAttributes() {
        // TODO: implement
        Model model = assertParseOk(
                "productline PL;"
                        + "features A,B,C,D;"
                        + "delta D when (0 < A.x < 10) && B.y;"
                );
        AppCond a = model.getProductLine().getDeltaClause(0).getAppCond();
    }

}
