/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.prettyprint;

import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintStream;
import java.io.PrintWriter;

import org.abs_models.Absc;
import org.abs_models.frontend.ast.Model;
import org.abs_models.frontend.parser.Main;
import org.stringtemplate.v4.STGroup;
import org.stringtemplate.v4.STGroupFile;

public class PrettyPrinterBackEnd extends Main {
    private File outputfile;

    public static STGroup templates;
    static {
        templates = new STGroupFile(PrettyPrinterBackEnd.class.getResource("/codegen/prettyprint.stg"));
    }

    public static int doMain(Absc args)  {
        PrettyPrinterBackEnd backend = new PrettyPrinterBackEnd();
        backend.arguments = args;
        int result = 0;
        try {
            result = backend.compile(args);
        } catch (Exception e) {
            System.err.println("An error occurred during compilation:\n" + e.getMessage());
            if (backend.arguments.debug) {
                e.printStackTrace();
            }
            result = 1;
        }
        return result;
    }

    public int compile(Absc args) throws Exception {
        this.arguments = args;
        final Model model = parse(arguments.files);
        if (arguments.prettyprint_keepsugar) {
            model.doAACrewrite = false;
            model.doForEachRewrite = false;
        }
        if (arguments.prettyprint_keepstdlib) {
            model.doPrettyPrintStdLib = true;
        }
        analyzeFlattenAndRewriteModel(model);
        if (!arguments.prettyprint_force && (model.hasParserErrors() || model.hasErrors() || model.hasTypeErrors())) {
            printErrorMessage();
            return 1;
        }

        // Set the line separator to LF so the file output prints UNIX line endings on println() calls.
        System.setProperty("line.separator", "\n");
        final PrintStream stream;
        final String loc;
        if (arguments.outputfile != null) {
            stream = new PrintStream(new FileOutputStream(arguments.outputfile), false, "utf-8");
            loc = arguments.outputfile.getAbsolutePath();
        } else {
            stream = System.out;
            loc = "Standard Output Stream";
        }

        if (arguments.verbose) {
            System.out.println("Output ABS model source code to " + loc + "...");
        }

        PrintWriter writer = new PrintWriter(new OutputStreamWriter(stream), true);
        // Set line separator back to default value
        System.setProperty("line.separator", System.lineSeparator());
        ABSFormatter formatter = new DefaultABSFormatter(writer);
        model.doPrettyPrint(writer, formatter);
        return 0;
    }
}
