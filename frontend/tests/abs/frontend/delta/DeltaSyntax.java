/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.delta;

import org.junit.Test;

import static org.junit.Assert.*;
import abs.frontend.ast.*;


public class DeltaSyntax extends DeltaTest {
    @Test
    public void delta() throws DeltaModellingException {
        Model model = assertParseOk("module M; delta D;");

        DeltaDecl delta = findDelta(model, "D");
        assertNotNull(delta);
    }

    @Test
    public void deltaUses() throws DeltaModellingException {
        Model model;
        model = assertParseOk("module M; delta D; uses M;");
        model = assertParseOk("module M; delta D; uses M; uses N;");
        model = assertParseOk("module M; delta D1; uses M; delta D2; uses M;");
        
    }

}
