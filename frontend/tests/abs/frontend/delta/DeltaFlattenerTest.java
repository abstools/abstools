/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.delta;

import org.junit.Test;
import static org.junit.Assert.*;

import abs.frontend.FrontendTest;
import abs.frontend.delta.exceptions.*;
import abs.frontend.ast.*;

public class DeltaFlattenerTest extends FrontendTest {
    @Test
    public void removeClass() throws ASTNodeNotFoundException {
        Model model = assertParseOk("module M; class C {} delta D { removes class C }");
        
        ClassDecl cls = null;
        for (Decl d : model.getDecls()) {
            if (d instanceof ClassDecl && d.getName().equals("C")) {
                cls = (ClassDecl) d;
                break;
            }
        }
        assertNotNull(cls);
        
        DeltaDecl delta = null;
        for (Decl d : model.getDecls()) {
            if (d instanceof DeltaDecl && d.getName().equals("D")) {
                delta = (DeltaDecl) d;
                break;
            }
        }
        assertNotNull(delta);
        
        model.applyDelta(delta);
        
        cls = null;
        for (ModuleDecl m : model.getModuleDecls()) {
            for (Decl d : m.getDecls()) {
                if (m.getName().equals("M") && d instanceof ClassDecl && d.getName().equals("C")) {
                    cls = (ClassDecl) d;
                    break;
                }
            }
        }
        assertNull(cls);
    }
    

    @Test
    public void addClass() throws ASTNodeNotFoundException {
        Model model = assertParseOk("module M; delta D { adds class C {} }");

        DeltaDecl delta = null;
        for (ModuleDecl m : model.getModuleDecls()) {
            for (Decl d : model.getDecls()) {
                if (m.getName().equals("M") && d instanceof DeltaDecl && d.getName().equals("D")) {
                    delta = (DeltaDecl) d;
                    break;
                }
            }
        }
        assertNotNull(delta);

        model.applyDelta(delta);

        ClassDecl cls = null;
        for (ModuleDecl m : model.getModuleDecls()) {
            for (Decl d : m.getDecls()) {
                if (m.getName().equals("M") && d instanceof ClassDecl && d.getName().equals("C")) {
                    cls = (ClassDecl) d;
                    break;
                }
            }
        }
        assertNotNull(cls);
    }
    
    
    // TODO test modifyClass
}
