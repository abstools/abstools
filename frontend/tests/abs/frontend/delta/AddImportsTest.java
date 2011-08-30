/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.delta;

import org.junit.Test;
import static org.junit.Assert.*;

import abs.frontend.delta.exceptions.*;
import abs.frontend.ast.*;


public class AddImportsTest extends DeltaFlattenerTest {
    @Test
    public void addUnqualImport() throws ASTNodeNotFoundException {
        Model model = assertParseOk(
                "module M1; export *;"
                + "class C {}"
                
                + "module M2; export *;"
                + "interface I { Unit m(); }"
                
                + "module MD;"
                + "import * from M1;"
                + "import * from M2;"
                + "delta D { "
                + "modifies class C implements I { adds Unit m() {} } "
                + "}"
        );
        
        // the compiler needs to add an "import I from M2" to M1 

    }
    
    
    @Test
    public void addQualImport() throws ASTNodeNotFoundException {
        Model model = assertParseOk(
                "module M1; export *;"
                + "class C {}"
                
                + "module M2; export *;"
                + "interface I { Unit m(); }"
                
                + "module MD;"
                + "import * from M1;"
                + "import * from M2;"
                + "delta D { "
                + "modifies class C implements M2.I { adds Unit m() {} } "
                + "}"
        );
        
        // the compiler needs to add an "import M2.I" to M1 

    }
}