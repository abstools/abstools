/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.delta;

import java.util.Collections;
import java.util.List;
import java.util.Set;

import org.junit.Test;

import static org.junit.Assert.*;
import abs.frontend.analyser.SemanticErrorList;
import abs.frontend.ast.Model;
import abs.frontend.ast.ProductLine;

public class DeltaTypeAnalysisTest extends DeltaTest {

    
    @Test
    public void stronglyUnambiguousProductLine() {
        Model model = assertParseOk(
                "module M;"
                        + "class C {}"
                        + "delta D1; modifies class M.C { adds Unit foo() {} }"
                        + "delta D2; modifies class M.C { adds Unit foo() {} }"
                        + "productline PL;"
                        + "features A;"
                        + "delta D1;"
                        + "delta D2 after D1;"
                );
        ProductLine pl = model.getProductLine();
        SemanticErrorList errors = new SemanticErrorList();
        assertTrue(pl.isStronglyUnambiguous(pl.getDeltaPartitions(errors), errors));
        
        model = assertParseOk(
                "module M;"
                        + "class C {}"
                        + "delta D1; modifies class M.C { adds Unit foo() {} }"
                        + "delta D2; modifies class M.C { adds Unit foo() {} }"
                        + "productline PL;"
                        + "features A;"
                        + "delta D1;"
                        + "delta D2;"
                );
        pl = model.getProductLine();
        errors = new SemanticErrorList();
        assertFalse(pl.isStronglyUnambiguous(pl.getDeltaPartitions(errors), errors));

        model = assertParseOk(
                "module M;"
                        + "class C {}"
                        + "delta D1; uses M; modifies class C { adds Unit foo() {} }"
                        + "delta D2; uses M; modifies class C { adds Unit foo() {} }"
                        + "productline PL;"
                        + "features A;"
                        + "delta D1;"
                        + "delta D2;"
                );
        pl = model.getProductLine();
        errors = new SemanticErrorList();
        assertFalse(pl.isStronglyUnambiguous(pl.getDeltaPartitions(errors), errors));

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
        List<Set<String>> partitions = pl.getDeltaPartitions(errors);
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
        partitions = pl.getDeltaPartitions(errors);
        assertTrue(partitions == Collections.<Set<String>>emptyList());
        
    }

}
