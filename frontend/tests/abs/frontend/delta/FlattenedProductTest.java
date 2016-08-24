/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.delta;

import static org.junit.Assert.*;

import org.junit.Test;

import abs.frontend.FrontendTest;
import abs.frontend.ast.*;

public class FlattenedProductTest extends FrontendTest {

    @Test
    public void test() {
        Model model = assertParseOk(
                "module M;"
                        + "class C {}"
                        + "delta D1; uses M; modifies class C { adds Unit foo() {} }"
                        + "delta D2; uses M;modifies class C { adds Unit foo() {} }"
                        + "productline PL;"
                        + "features A;"
                        + "delta D1;"
                        + "delta D2 after D1;"
                );

        InstModel imodel = model.getInstModel();
        assertTrue(imodel.getModel() == model);

        List<InstNode> l = imodel.getInstNodeList();
        
        List<InstNode> l1 = imodel.getInstNodeList();
        List<InstNode> l2 = imodel.getInstNodeList();

        System.out.println("*** list " + l);

        for( InstNode node : l) {
            System.out.println("***" + node);
        }
    }

}
