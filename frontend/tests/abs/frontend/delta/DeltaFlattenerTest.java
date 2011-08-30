/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.delta;

import abs.frontend.FrontendTest;
import abs.frontend.ast.*;

public class DeltaFlattenerTest extends FrontendTest {
    
    // helper method: find a Decl node in given module
    protected Decl findDecl(Model model, String moduleName, String name) {
        Decl decl = null;
        out: for (ModuleDecl m : model.getModuleDecls()) {
            for (Decl d : m.getDecls()) {
                if (m.getName().equals(moduleName) && d.getName().equals(name)) {
                    decl = d;
                    break out;
                }
            }
        }
        return decl;
    }

}
