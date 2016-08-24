/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.delta;

import java.util.List;
import java.util.Set;
import java.util.UUID;

import org.junit.Test;

import static org.junit.Assert.*;
import abs.frontend.analyser.SemanticConditionList;
import abs.frontend.ast.*;

public class ProductLineTypeAnalysisTest extends DeltaTest {


    @Test
    public void stronglyUnambiguousProductLine() {
        Model model = assertParseOk(
                "module M;"
                        + "delta D1; uses M; modifies class C { adds Unit foo() {} }"
                        + "delta D2; modifies class M.C { adds Unit foo() {} }"
                        + "productline PL;"
                        + "features A;"
                        + "delta D1;"
                        + "delta D2 after D1;"
                );
        ProductLine pl = model.getProductLine();
        SemanticConditionList errors = new SemanticConditionList();

        assertTrue(ProductLineTypeAnalysisHelper.isStronglyUnambiguous(pl, errors));
        assertEquals(0, errors.getErrorCount());

        model = assertParseOk(
                "module M;"
                        + "delta D1; uses M; modifies class C { adds Unit foo() {} }"
                        + "delta D2; modifies class M.C { adds Unit bar() {} }"
                        + "productline PL;"
                        + "features A;"
                        + "delta D1;"
                        + "delta D2;"
                );
        pl = model.getProductLine();
        errors = new SemanticConditionList();

        assertTrue(ProductLineTypeAnalysisHelper.isStronglyUnambiguous(pl, errors));
        assertEquals(0, errors.getErrorCount());

        model = assertParseOk(
                "module M;"
                        + "productline PL;"
                        + "features A;"
                );
        pl = model.getProductLine();
        errors = new SemanticConditionList();

        assertTrue(ProductLineTypeAnalysisHelper.isStronglyUnambiguous(pl, errors));
        assertEquals(0, errors.getErrorCount());

        List<Set<String>> partition = pl.getDeltaPartition();
        assertEquals(partition.size(), 0);

        model = assertParseOk(
                "module M;"
                        + "delta D1; uses M; modifies class C { adds Unit foo() {} }"
                        + "delta D2; modifies class M.C { adds Unit foo() {} }"
                        + "productline PL;"
                        + "features A;"
                        + "delta D1;"
                        + "delta D2;"
                );
        pl = model.getProductLine();
        errors = new SemanticConditionList();

        assertFalse(ProductLineTypeAnalysisHelper.isStronglyUnambiguous(pl, errors));
        assertEquals(1, errors.getErrorCount());

        model = assertParseOk(
                "module M;"
                        + "delta D1; uses M; modifies class C { adds Unit foo() {} }"
                        + "delta D2; modifies class M.C { adds Unit foo() {} }"
                        + "productline PL;"
                        + "features A;"
                        + "delta D1;"
                        + "delta D2;"
                );
        pl = model.getProductLine();
        errors = new SemanticConditionList();

        assertFalse(ProductLineTypeAnalysisHelper.isStronglyUnambiguous(pl, errors));
        assertTrue(errors.getErrorCount() == 1);

        model = assertParseOk(
                "module M;"
                        + "delta D1; uses M; adds class C {}"
                        + "delta D2; removes class M.C;"
                        + "productline PL;"
                        + "features A;"
                        + "delta D1;"
                        + "delta D2;"
                );
        pl = model.getProductLine();
        errors = new SemanticConditionList();

        assertFalse(ProductLineTypeAnalysisHelper.isStronglyUnambiguous(pl, errors));
        assertEquals(1, errors.getErrorCount());

        model = assertParseOk(
                "module M;"
                        + "delta D1; modifies class M.C { adds Unit foo() {} }"
                        + "delta D2; uses M; removes class C;"
                        + "delta D3; uses  M; adds class C {}"
                        + "productline PL;"
                        + "features A;"
                        + "delta D1;"
                        + "delta D2;"
                        + "delta D3;"
                );
        pl = model.getProductLine();
        errors = new SemanticConditionList();

        assertFalse(ProductLineTypeAnalysisHelper.isStronglyUnambiguous(pl, errors));
        assertEquals(2, errors.getErrorCount());

    }

    @Test
    public void stronglyUnambiguousProductLinePerformance() {
        Model model = assertParseOk("module M; class C {}");
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
            ModifyMethodModifier mmod = new ModifyMethodModifier();
            mmod.setMethodImpl(mimpl);

            cu.addDeltaDecl(new DeltaDecl("D" + id,
                    new abs.frontend.ast.List<DeltaParamDecl>(),
                    new abs.frontend.ast.List<DeltaAccess>(),
                    new abs.frontend.ast.List<ModuleModifier>(cmod)));

            DeltaClause dc = new DeltaClause();
            Deltaspec dspec = new Deltaspec();
            dspec.setDeltaID("D" + id);
            dc.setDeltaspec(dspec);
            pl.addDeltaClause(dc);
        }

        pl = model.getProductLine();
        SemanticConditionList errors = new SemanticConditionList();

        long startTime = System.currentTimeMillis();
        assertTrue(ProductLineTypeAnalysisHelper.isStronglyUnambiguous(pl, errors));
        long stopTime = System.currentTimeMillis();
        //System.out.println(n + " deltas. time (ms): " + (stopTime - startTime));
    }


//    @Test
//    public void ntaTest() {
//        Model model = new Model();
//        assertEquals(1, model.getTestList().getNumChild());
//    }

    @Test
    public void Products1() {
        Model model = assertParseOk(
                "root FM {"
                        + " group allof { A }"
                        + "}"
                );
        //one single product
        assertEquals(1, model.getProductList().getNumChild());
    }

    @Test
    public void ProductsIgnoreAttributes() {
        Model model = assertParseOk(
                "root FM {"
                        + "group allof { opt A { ifin: FM.attr == 99; ifout: FM.attr == 14; } }"
                        + "Int attr;"
                        + "}"
                );
        //two products: {FM}, {FM, A}
        System.out.println("Model Products: " + model.getProductList().getNumChild());
        assertEquals(2, model.getProductList().getNumChild());
    }

    @Test
    public void ProductsMany() {
        Model model = assertParseOk(
                "root FM {"
                        + "group [0..*] { A, B, C, D, E, F, G, H, I, J }"
                        + "}"
                );
        //with 10 features there should be 2^10 valid products
        assertEquals(1024, model.getProductList().getNumChild());
    }
}
