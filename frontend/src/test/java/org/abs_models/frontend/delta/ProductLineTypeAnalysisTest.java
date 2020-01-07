/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.frontend.delta;

import java.util.List;
import java.util.Set;
import java.util.UUID;

import org.abs_models.frontend.analyser.SemanticConditionList;
import org.abs_models.frontend.ast.*;
import org.junit.Test;

import static org.junit.Assert.*;

public class ProductLineTypeAnalysisTest extends DeltaTest {

    @Test
    public void wellFormedProductLine1() {
        Model model = assertParse("module M;"
            + "delta D1;"
            + "delta D2;"
            + "productline PL;"
            + "features A;"
            + "delta D1 after D2;");
        ProductLine pl = model.getProductLine();
        SemanticConditionList errors = new SemanticConditionList();

        /*
         * No delta clause for D2
         */
        assertFalse(ProductLineAnalysisHelper.wellFormedProductLine(pl, errors));
        assertEquals(1, errors.getErrorCount());
    }

    @Test
    public void wellFormedProductLine2() {
        Model model = assertParse("module M;"
            + "delta D1;"
            + "delta D2;"
            + "productline PL;"
            + "features A;"
            + "delta D1 after D2;"
            + "delta D2;");
        ProductLine pl = model.getProductLine();
        SemanticConditionList errors = new SemanticConditionList();

        /*
         * All OK
         */
        assertTrue(ProductLineAnalysisHelper.wellFormedProductLine(pl, errors));
        assertEquals(0, errors.getErrorCount());
    }

    @Test
    public void stronglyUnambiguousProductLine1() {
        Model model = assertParse("module M;"
            + "delta D1; uses M; modifies class C { adds Unit foo() {} }"
            + "delta D2; modifies class M.C { adds Unit foo() {} }"
            + "productline PL;"
            + "features A;"
            + "delta D1;"
            + "delta D2 after D1;");
        ProductLine pl = model.getProductLine();
        SemanticConditionList errors = new SemanticConditionList();

        assertTrue(ProductLineAnalysisHelper.isStronglyUnambiguous(pl, errors));
        assertEquals(0, errors.getErrorCount());
    }

    @Test
    public void stronglyUnambiguousProductLine2() {
        Model model = assertParse("module M;"
            + "delta D1; uses M; modifies class C { adds Unit foo() {} }"
            + "delta D2; uses M; modifies class C { adds Unit bar() {} }"
            + "productline PL;"
            + "features A;"
            + "delta D1;"
            + "delta D2;");
        ProductLine pl = model.getProductLine();
        SemanticConditionList errors = new SemanticConditionList();

        /*
         * Both deltas are in same partition (no order specified)
         * They modify class C in different ways, by adding different methods
         */
        assertTrue(ProductLineAnalysisHelper.isStronglyUnambiguous(pl, errors));
        assertEquals(0, errors.getErrorCount());
    }

    @Test
    public void stronglyUnambiguousProductLine3() {
        Model model = assertParse("module M;"
            + "productline PL;"
            + "features A;");
        ProductLine pl = model.getProductLine();
        SemanticConditionList errors = new SemanticConditionList();

        /*
         * No deltas, no worries
         */
        assertTrue(ProductLineAnalysisHelper.isStronglyUnambiguous(pl, errors));
        assertEquals(0, errors.getErrorCount());

        List<Set<String>> partition = pl.getDeltaPartition();
        assertEquals(partition.size(), 0);
    }

    @Test
    public void stronglyUnambiguousProductLine4() {
        Model model = assertParse("module M;"
            + "delta D1; uses M; modifies class C { adds Unit foo() {} }"
            + "delta D2; uses M; modifies class C { adds Unit foo() {} }"
            + "productline PL;"
            + "features A;"
            + "delta D1;"
            + "delta D2;");
        ProductLine pl = model.getProductLine();
        SemanticConditionList errors = new SemanticConditionList();

        /* Both deltas are in same partition (no order specified)
         * Both deltas modify the same method C.foo
         */
        assertFalse(ProductLineAnalysisHelper.isStronglyUnambiguous(pl, errors));
        assertEquals(1, errors.getErrorCount());
    }

    @Test
    public void stronglyUnambiguousProductLine5() {
        Model model = assertParse("module M;"
            + "delta D1; uses M; adds class C {}"
            + "delta D2; uses M; removes class C;"
            + "productline PL;"
            + "features A;"
            + "delta D1;"
            + "delta D2;");
        ProductLine pl = model.getProductLine();
        SemanticConditionList errors = new SemanticConditionList();

        /* Both deltas are in same partition (no order specified)
         * Both deltas act on the same class C
         */
        assertFalse(ProductLineAnalysisHelper.isStronglyUnambiguous(pl, errors));
        assertEquals(1, errors.getErrorCount());
    }

    @Test
    public void stronglyUnambiguousProductLine6() {
        Model model = assertParse("module M;"
            + "delta D1; uses M; modifies class C { adds Unit foo() {} }"
            + "delta D2; uses M; removes class C;"
            + "delta D3; uses M; adds class C {}"
            + "productline PL;"
            + "features A;"
            + "delta D1;"
            + "delta D2;"
            + "delta D3;");
        ProductLine pl = model.getProductLine();
        SemanticConditionList errors = new SemanticConditionList();

        /* All 3 deltas are in same partition
         * D1, D2, D3 act on the same class C
         */
        assertFalse(ProductLineAnalysisHelper.isStronglyUnambiguous(pl, errors));
        assertEquals(2, errors.getErrorCount());
    }

    @Test
    public void stronglyUnambiguousProductLinePerformance() {
        Model model = assertParse("module M; class C {}");
        CompilationUnit cu = new CompilationUnit();
        model.addCompilationUnit(cu);

        ProductLine pl = new ProductLine();
        pl.setName("PL");
        cu.setProductLine(pl);

        int n = 2000;
        for (int i=0; i<n; i++) {

            String id = Integer.toHexString(UUID.randomUUID().hashCode());

            ModifyClassModifier cmod = new ModifyClassModifier();
            cmod.setName("C");
            MethodSig msig = new MethodSig();
            msig.setName("m" + id);
            MethodImpl mimpl = new MethodImpl();
            mimpl.setMethodSig(msig);
            org.abs_models.frontend.ast.List<MethodImpl> list = new org.abs_models.frontend.ast.List<>();
            list.add(mimpl);
            DeltaTraitModifier dmod = new DeltaTraitModifier(new ModifyMethodModifier(new TraitSetExpr(list)));

            cu.addDeltaDecl(new DeltaDecl("D" + id,
                new org.abs_models.frontend.ast.List<>(),
                new org.abs_models.frontend.ast.Opt<>(),
                new org.abs_models.frontend.ast.List<>(cmod)));

            DeltaClause dc = new DeltaClause();
            Deltaspec dspec = new Deltaspec();
            dspec.setDeltaID("D" + id);
            dc.setDeltaspec(dspec);
            pl.addDeltaClause(dc);
        }

        pl = model.getProductLine();
        SemanticConditionList errors = new SemanticConditionList();

        long startTime = System.currentTimeMillis();
        assertTrue(ProductLineAnalysisHelper.isStronglyUnambiguous(pl, errors));
        long stopTime = System.currentTimeMillis();
        //System.out.println(n + " deltas. time (ms): " + (stopTime - startTime));
    }

}
