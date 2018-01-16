/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.prettyprint;

import java.io.File;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.List;

import abs.common.NotImplementedYetException;
import abs.frontend.ast.Model;
import abs.frontend.parser.Main;
import abs.frontend.tests.ABSFormatter;

public class PrettyPrinterBackEnd extends Main {
    private File outputfile;
    private boolean force = false;
    private boolean keepsugar = false;

    public static void main(final String... args) {
        PrettyPrinterBackEnd backEnd = new PrettyPrinterBackEnd();
        try {
            backEnd.compile(args);
        } catch (NotImplementedYetException e) {
            System.err.println(e.getMessage());
            System.exit(1);
        } catch (Exception e) {
            System.err.println("An error occurred during compilation:\n" + e.getMessage());

            if (backEnd.debug) {
                e.printStackTrace();
            }

            System.exit(1);
        }
    }

    @Override
    public List<String> parseArgs(String[] args) {
        List<String> restArgs = super.parseArgs(args);
        List<String> remainingArgs = new ArrayList<>();

        for (int i = 0; i < restArgs.size(); i++) {
            String arg = restArgs.get(i);
            if (arg.equals("-o")) {
                i++;
                if (i == restArgs.size()) {
                    System.err.println("Please provide an output file");
                    System.exit(1);
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
    public void compile(String[] args) throws Exception {
        final Model model = parseFiles(parseArgs(args).toArray(new String[0]));
        if (keepsugar) {
            model.doAACrewrite = false;
            model.doForEachRewrite = false;
        }
        analyzeModel(model);
        if (! force && (model.hasParserErrors() || model.hasErrors() || model.hasTypeErrors())) {
            printParserErrorAndExit();
        }

        final PrintStream stream;
        final String loc;
        if (outputfile != null) {
            stream = new PrintStream(outputfile);
            loc = outputfile.getAbsolutePath();
        } else {
            stream = System.out;
            loc = "Standard Output Stream";
        }

        if (verbose) {
            System.out.println("Output ABS model source code to "+loc+"...");
        }

        PrintWriter writer = new PrintWriter(stream,true);
        ABSFormatter formatter = new DefaultABSFormatter(writer);
        model.doPrettyPrint(writer, formatter);
    }

    public static void printUsage() {
        System.out.println("ABS Pretty Printer (-prettyprint):\n"
                + "  -f             force pretty printing even if there are type errors\n"
                + "  -keepsugar     do not transform statements into basic core abs\n"
                + "  -o <file>      write output to <file> instead of standard output\n"
        );
    }

}
