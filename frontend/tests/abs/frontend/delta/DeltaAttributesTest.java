/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.delta;

import org.junit.Test;

import abs.frontend.ast.DeltaDecl;
import abs.frontend.ast.Model;
import abs.frontend.delta.exceptions.ASTNodeNotFoundException;


public class DeltaAttributesTest extends DeltaFlattenerTest {

    @Test
    public void addFeatureAsAttribute() throws ASTNodeNotFoundException {
        Model model = assertParseOk(
                "module M; " +
                "delta D(Boolean b) {}" +
                "productline PL { features F1, F2; delta D(F2) when F1; } " +
                "product P1(F1); " +
                "product P2(F1,F2);"
        );
        
        DeltaDecl delta = (DeltaDecl) findDecl(model, "M", "D");
        model.applyDelta(delta);
        
        
    }
}
