/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.maude;

import java.io.File;
import java.io.InputStream;
import java.io.PrintStream;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.ArrayList;
import java.util.List;

import com.google.common.io.ByteStreams;

import abs.common.NotImplementedYetException;
import abs.frontend.ast.Model;
import abs.frontend.parser.Main;

public class MaudeCompiler extends Main {

    public enum SIMULATOR {
        RL(SIMULATOR_RL), EQ_TIMED(SIMULATOR_EQ_TIMED);
        private final String module;
        SIMULATOR(String arg) {
            this.module = arg;
        }
        public String getModule() {
            return module;
        }
    };

    private static final String SIMULATOR_RL = "ABS-SIMULATOR-RL";
    private static final String SIMULATOR_EQ_TIMED = "ABS-SIMULATOR-EQ-TIMED";
    private static final String RUNTIME_INTERPRETER_PATH
        = "abs/backend/maude/abs-interpreter.maude";

    SIMULATOR module = SIMULATOR.RL;
    private File outputfile;
    private String mainBlock;
    private int clocklimit = 100;
    private int defaultResources = 0;
    private static boolean debug = false;

    public static void main(final String... args) {
        /* Maude has build-in AwaitAsyncCall support */
        Model.doAACrewrite = false;
        try {
            new MaudeCompiler().compile(args);
        } catch (NotImplementedYetException e) {
            System.err.println(e.getMessage());
            System.exit(0);
        } catch (Exception e) {
            System.err.println("An error occurred during compilation:\n" + e.getMessage());

            if (debug) {
                e.printStackTrace();
            }

            System.exit(1);
        }
    }

    @Override
    public List<String> parseArgs(String[] args) {
        List<String> restArgs = super.parseArgs(args);
        List<String> remainingArgs = new ArrayList<String>();

        for (int i = 0; i < restArgs.size(); i++) {
            String arg = restArgs.get(i);
            if (arg.equals("-timed")) {
                module = SIMULATOR.EQ_TIMED;
            } else if (arg.equals("-o")) {
                i++;
                if (i == restArgs.size()) {
                    System.err.println("Please provide an output file");
                    System.exit(1);
                } else {
                    outputfile = new File(restArgs.get(i));
                }
            } else if (arg.startsWith("-main=")) {
                mainBlock = arg.split("=")[1];
            } else if (arg.startsWith("-limit=")) {
                // -limit implies -timed
                module = SIMULATOR.EQ_TIMED;
                clocklimit = Integer.parseInt(arg.split("=")[1]);
            } else if (arg.startsWith("-defaultcost=")) {
                defaultResources = Integer.parseInt(arg.split("=")[1]);
            } else if (arg.startsWith("-debug")) {
                debug  = true;
            } else if (arg.equals("-maude")) {
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
        if (verbose) System.out.println("Generating Erlang code...");
        final Model model = parse(args);
        if (model.hasParserErrors()
            || model.hasErrors()
            || model.hasTypeErrors())
            printParserErrorAndExit();

        PrintStream stream = System.out;
        if (outputfile != null) {
            stream = new PrintStream(outputfile);
        }
        InputStream is = ClassLoader.getSystemResourceAsStream(RUNTIME_INTERPRETER_PATH);
        if (is == null)
            throw new RuntimeException("Could not locate abs-interpreter.maude");
        DateFormat dateFormat = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss");

        stream.println("*** Generated " + dateFormat.format(new Date()));
        ByteStreams.copy(is, stream);
        model.generateMaude(stream, module, mainBlock, clocklimit, defaultResources);
        if (verbose) System.out.println("Finished.  Start `maude " + outputfile.toString() + "' to run the model.");
    }

    protected void printUsage() {
        super.printUsage();
        System.out.println("Maude Backend:\n"
                + "  -main=<ModuleName> \n"
                + "                 sets the main block to execute\n"
                + "  -o <file>      write output to <file> instead of standard output\n"
                + "  -timed         generate code for timed interpreter\n"
                + "  -limit=n       set clock limit for timed interpreter to n (default 100)\n"
                + "                 (implies -timed)\n"
                + "  -defaultcost=n set default statement execution cost (default 0)\n"
                + "  -debug         print stacktrace on exception\n"
        );
    }

}

// Local Variables:
// tab-width: 4
// End:
