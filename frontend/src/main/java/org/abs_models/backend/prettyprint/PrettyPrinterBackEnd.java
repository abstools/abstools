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
import java.util.ArrayList;
import java.util.List;

import org.abs_models.backend.common.InternalBackendException;
import org.abs_models.common.NotImplementedYetException;
import org.abs_models.frontend.ast.Model;
import org.abs_models.frontend.parser.Main;
import org.abs_models.frontend.tests.ABSFormatter;

public class PrettyPrinterBackEnd extends Main {
    private File outputfile;
    private boolean force = false;
    private boolean keepsugar = false;

    public static void main(final String... args) {
        doMain(args);
    }

    public static int doMain(final String... args) {
        int result = 0;
        PrettyPrinterBackEnd backEnd = new PrettyPrinterBackEnd();
        try {
            result = backEnd.compile(args);
        } catch (NotImplementedYetException e) {
            System.err.println(e.getMessage());
            result = 1;
        } catch (Exception e) {
            System.err.println("An error occurred during compilation:\n" + e.getMessage());

            if (backEnd.debug) {
                e.printStackTrace();
            }

            result = 1;
        }
        return result;
    }

    @Override
    public List<String> parseArgs(String[] args) throws InternalBackendException {
        List<String> restArgs = super.parseArgs(args);
        List<String> remainingArgs = new ArrayList<>();

        for (int i = 0; i < restArgs.size(); i++) {
            String arg = restArgs.get(i);
            if (arg.equals("-o")) {
                i++;
                if (i == restArgs.size()) {
                    throw new InternalBackendException("Missing output file name after '-o'");
                } else {
                    outputfile = new File(restArgs.get(i));
                    if (outputfile.exists()) {
                        outputfile.delete();
                    }
                }
            } else if (arg.equals("-f"))  {
                force = true;
            } else if (arg.equals("-keepsugar"))  {
                keepsugar = true;
            } else if (arg.equals("-prettyprint")) {
                // nothing to do
            } else {
                remainingArgs.add(arg);
            }
        }

        return remainingArgs;
    }

    /**
     * @param args
     * @throws Exception
     */
    public int compile(String[] args) throws Exception {
        final Model model = parseFiles(verbose, stdlib, parseArgs(args).toArray(new String[0]));
        if (keepsugar) {
            model.doAACrewrite = false;
            model.doForEachRewrite = false;
        }
        analyzeFlattenAndRewriteModel(model);
        if (!force && (model.hasParserErrors() || model.hasErrors() || model.hasTypeErrors())) {
            printErrorMessage();
            return 1;
        }

        // Set the line separator to LF so the file output prints UNIX line endings on println() calls.
        System.setProperty("line.separator", "\n");
        final PrintStream stream;
        final String loc;
        if (outputfile != null) {
            stream = new PrintStream(new FileOutputStream(outputfile), false, "utf-8");
            loc = outputfile.getAbsolutePath();
        } else {
            stream = System.out;
            loc = "Standard Output Stream";
        }

        if (verbose) {
            System.out.println("Output ABS model source code to " + loc + "...");
        }

        PrintWriter writer = new PrintWriter(new OutputStreamWriter(stream), true);
        // Set line separator back to default value
        System.setProperty("line.separator", System.lineSeparator());
        ABSFormatter formatter = new DefaultABSFormatter(writer);
        model.doPrettyPrint(writer, formatter);
        return 0;
    }

    public static void printUsage() {
        System.out.println("ABS Pretty Printer (-prettyprint):\n"
                + "  -f             force pretty printing even if there are type errors\n"
                + "  -keepsugar     do not transform statements into basic core abs\n"
                + "  -o <file>      write output to <file> instead of standard output\n"
        );
    }

}
