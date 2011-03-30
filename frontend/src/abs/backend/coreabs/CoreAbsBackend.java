/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.coreabs;

import java.io.PrintStream;

import abs.frontend.ast.Model;
import abs.frontend.parser.Main;

public class CoreAbsBackend extends Main {

    
    public CoreAbsBackend() {
        super();
        // TODO Auto-generated constructor stub
    }

    /**
     * @param args
     */
    public static void main(final String... args) throws Exception {

        Model m = new CoreAbsBackend().parse(args);
        PrintStream stream = System.out;
        m.generateCoreABS(stream);
        System.exit(0);
    }

}
