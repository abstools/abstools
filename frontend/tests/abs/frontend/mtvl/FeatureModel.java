/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.mtvl;

import org.junit.Test;

import abs.frontend.FrontendTest;
import abs.frontend.ast.Model;

public class FeatureModel extends FrontendTest {

    @Test
    public void minimalFM() {
        Model model = assertParseOk(
                "root FM"
        );
    }
    @Test
    public void minimalFMwithProduct() {
        Model model = assertParseOk(
                "product P (FM);"
                + "root FM"
        );
    }


}
