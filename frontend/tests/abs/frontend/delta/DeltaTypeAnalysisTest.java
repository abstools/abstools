/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.delta;

import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.UUID;

import org.junit.Test;

import static org.junit.Assert.*;
import abs.frontend.analyser.SemanticErrorList;
import abs.frontend.ast.*;

public class DeltaTypeAnalysisTest extends DeltaTest {

    
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
        SemanticErrorList errors = new SemanticErrorList();
        assertTrue(pl.isStronglyUnambiguous(pl.getDeltaPartition(errors), errors));
        assertTrue(errors.size() == 0);
        
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
        errors = new SemanticErrorList();
        assertTrue(pl.isStronglyUnambiguous(pl.getDeltaPartition(errors), errors));
        assertTrue(errors.size() == 0);

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
        errors = new SemanticErrorList();
        assertFalse(pl.isStronglyUnambiguous(pl.getDeltaPartition(errors), errors));
        assertTrue(errors.size() == 1);
        System.out.println(errors);
        
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
        errors = new SemanticErrorList();
        assertFalse(pl.isStronglyUnambiguous(pl.getDeltaPartition(errors), errors));
        assertTrue(errors.size() == 1);
        
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
        errors = new SemanticErrorList();
        assertFalse(pl.isStronglyUnambiguous(pl.getDeltaPartition(errors), errors));
        assertTrue(errors.size() == 1);

        model = assertParseOk(
                "module M;"
                        + "delta D1; modifies class M.C { adds Unit foo() {} }"
                        + "delta D2; removes class M.C;"
                        + "delta D3; adds class M.C {}"
                        + "productline PL;"
                        + "features A;"
                        + "delta D1;"
                        + "delta D2;"
                        + "delta D3;"
                );
        pl = model.getProductLine();
        errors = new SemanticErrorList();
        assertFalse(pl.isStronglyUnambiguous(pl.getDeltaPartition(errors), errors));
        assertTrue(errors.size() == 2);
        
    }

    @Test
    public void stronglyUnambiguousProductLinePerformance() {
        Model model = assertParseOk("module M; class C {}");
        CompilationUnit cu = new CompilationUnit();
        model.addCompilationUnit(cu);
        
        ProductLine pl = new ProductLine();
        cu.setProductLine(pl);
        
        int n = 10;
        for (int i=0; i<n; i++) {
            
            String id = Integer.toHexString(UUID.randomUUID().hashCode());
            
            AddClassModifier cmod = new AddClassModifier();
            ClassDecl cls = new ClassDecl();
            cls.setName("C");
            cmod.setClassDecl(cls);
            MethodSig msig = new MethodSig();
//            msig.setName("m" + id);
            msig.setName("m");
            MethodImpl mimpl = new MethodImpl();
            mimpl.setMethodSig(msig);
            cls.addMethod(mimpl);
                                
            cu.addDeltaDecl(new DeltaDecl("D" + id,
                    new abs.frontend.ast.List<DeltaParamDecl>(),
                    new abs.frontend.ast.List<DeltaAccess>(),
                    new abs.frontend.ast.List<ModuleModifier>(cmod)));

            DeltaClause dc = new DeltaClause();
            Deltaspec dspec = new Deltaspec();
            dspec.setName("D" + id);
            dc.setDeltaspec(dspec);
            pl.addDeltaClause(dc);
        }
        
        System.out.println("Deltas: " + model.getDeltaDecls().size());
        pl = model.getProductLine();
        SemanticErrorList errors = new SemanticErrorList();
        System.out.println(pl.getDeltaPartition(errors).get(0).size());
        assertTrue(pl.isStronglyUnambiguous(pl.getDeltaPartition(errors), errors));

    }

    
    @Test
    public void deltaPartitions() {
        Model model = assertParseOk(
                "module M;"
                        + "class C {}"
                        + "delta D1; modifies class M.C { adds Unit foo() {} }"
                        + "delta D2; modifies class M.C { adds Unit foo() {} }"
                        + "productline PL;"
                        + "features A;"
                        + "delta D1;"
                        + "/* D2 clause mising */"
                );
        ProductLine pl = model.getProductLine();
        SemanticErrorList errors = new SemanticErrorList();
        List<Set<String>> partitions = pl.getDeltaPartition(errors);
        assertTrue(partitions == Collections.<Set<String>>emptyList());

        model = assertParseOk(
                "module M;"
                        + "class C {}"
                        + "delta D1; modifies class M.C { adds Unit foo() {} }"
                        + "delta D2; modifies class M.C { adds Unit foo() {} }"
                        + "productline PL;"
                        + "features A;"
                        + "delta D1 after D2;"
                        + "delta D2 after D1; // cycle!"
                );
        pl = model.getProductLine();
        errors = new SemanticErrorList();
        partitions = pl.getDeltaPartition(errors);
        assertTrue(partitions == Collections.<Set<String>>emptyList());
        
    }

}
