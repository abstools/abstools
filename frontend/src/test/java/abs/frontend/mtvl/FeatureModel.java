/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.mtvl;

import org.junit.Test;

import abs.common.WrongProgramArgumentException;
import abs.frontend.FrontendTest;
import abs.frontend.ast.Model;
import abs.frontend.delta.DeltaModellingException;

public class FeatureModel extends FrontendTest {

    @Test
    public void minimalFM() {
        Model model = assertParseOk(
                "root FM"
        );
    }

    @Test
    public void minimalFMwithProduct() throws DeltaModellingException, WrongProgramArgumentException {
        Model model = assertParseOk(
                "productline PL; features FM;"
                + "product P(FM);"
                + "root FM"
        );
        model.flattenForProduct("P");
    }

    @Test
    public void attributeUnbounded() throws DeltaModellingException, WrongProgramArgumentException {
        Model model = assertParseOk(
                "productline PL; features FM;"
                + "product P();"
                + "root FM { Int attr; }"
        );
        model.flattenForProduct("P");
    }

    @Test
    public void attributeUnbounded2() throws DeltaModellingException, WrongProgramArgumentException {
        Model model = assertParseOk(
                "productline PL; features FM;"
                + "product P();"
                + "root FM { Int attr in [*..*]; }"
        );
        model.flattenForProduct("P");
    }

    @Test
    public void attributeBounded() throws DeltaModellingException, WrongProgramArgumentException {
        Model model = assertParseOk(
                "productline PL; features FM;"
                + "product P();"
                + "root FM { Int attr in [0..99]; }"
        );
        model.flattenForProduct("P");
    }


}
