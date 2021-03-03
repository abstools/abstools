/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.outline;

import java.io.File;
import java.io.PrintStream;
import java.io.PrintWriter;

import org.abs_models.Absc;
import org.abs_models.common.NotImplementedYetException;
import org.abs_models.frontend.ast.Model;
import org.abs_models.frontend.parser.Main;

public class OutlinePrinterBackEnd extends Main {
    private boolean force = false;

    public static int doMain(Absc args) {
        int result = 0;
        OutlinePrinterBackEnd backEnd = new OutlinePrinterBackEnd();
        backEnd.arguments = args;
        try {
            result = backEnd.compile();
        } catch (NotImplementedYetException e) {
            System.err.println(e.getMessage());
            result = 1;
        } catch (Exception e) {
            System.err.println("An error occurred during compilation:\n" + e.getMessage());

            if (backEnd.arguments.debug) {
                e.printStackTrace();
            }

            result = 1;
        }
        return result;
    }

    public int compile() throws Exception {
        final Model model = parse(arguments.files);
        if (! force && (model.hasParserErrors() || model.hasErrors() || model.hasTypeErrors())) {
            printErrorMessage();
            return 1;
        }

        PrintStream stream = System.out;
        String loc = "Standard Output Stream";
        if (arguments.outputfile != null) {
            stream = new PrintStream(arguments.outputfile);
            loc = arguments.outputfile.getAbsolutePath();
        }

        if (arguments.verbose) {
            System.out.println("Output ABS model source code to "+loc+"...");
        }

        PrintWriter writer = new PrintWriter(stream,true);
        model.doOutlinePrint(writer, "");
        return 0;
    }
}
