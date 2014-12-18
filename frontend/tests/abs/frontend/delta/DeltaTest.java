/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.delta;

import abs.frontend.FrontendTest;
import abs.frontend.ast.*;

public class DeltaTest extends FrontendTest {

    // helper method: find a Decl node in given module
    static protected Decl findDecl(Model model, String moduleName, String name) {
        for (ModuleDecl m : model.getModuleDecls()) {
            if (m.getName().equals(moduleName))
                for (Decl d : m.getDecls()) {
                    if (d.getName().equals(name))
                        return d;
                }
        }
        return null;
    }

    protected DeltaDecl findDelta(Model model, String name) {
        return model.findDelta(name);
    }
    
}
