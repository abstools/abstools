/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.frontend.delta;

import org.abs_models.frontend.ast.DeltaDecl;
import org.abs_models.frontend.ast.Model;
import org.junit.Test;

import static org.junit.Assert.*;

public class DeltaSyntax extends DeltaTest {
    @Test
    public void delta() throws DeltaModellingException {
        Model model = assertParse("module M; delta D;");

        DeltaDecl delta = findDelta(model, "D");
        assertNotNull(delta);
    }

    @Test
    public void deltaUses() throws DeltaModellingException {
        assertParse("module M; delta D; uses M;");
        assertParse("module M; delta D1; uses M; delta D2; uses M;");
        assertParse("module M; delta D; adds interface M.I {}");
    }

}
