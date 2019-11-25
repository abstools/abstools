/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.coreabs;

import java.io.PrintStream;

import org.abs_models.Absc;
import org.abs_models.frontend.ast.Model;
import org.abs_models.frontend.parser.Main;

public class CoreAbsBackend extends Main {

    public int mainMethod(Absc args) {
        this.arguments = args;
        try {
            Model m = parse(arguments.files);
            if (m == null) {
                printErrorMessage();
                return 1;
            }
            PrintStream stream = System.out;
            String loc = "Standard Output";
            if (arguments.outputfile != null) {
                stream = new PrintStream(arguments.outputfile);
                loc = arguments.outputfile.getAbsolutePath();
            }
            if (arguments.verbose) {
                System.out.println("Generating Core ABS to "+loc+"...");
            }
            m.generateCoreABS(stream);
            return 0;
        } catch (Exception e) {
            printError(e.getMessage());
            return 1;
        }
    }

    public static int doMain(Absc args)  {
        return new CoreAbsBackend().mainMethod(args);
    }

}
