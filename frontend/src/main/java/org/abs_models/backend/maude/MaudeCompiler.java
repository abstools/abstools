/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.maude;

import java.io.File;
import java.io.InputStream;
import java.io.PrintStream;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import com.google.common.io.ByteStreams;

import org.abs_models.backend.common.InternalBackendException;
import org.abs_models.common.NotImplementedYetException;
import org.abs_models.frontend.ast.Model;
import org.abs_models.frontend.parser.Main;
import org.stringtemplate.v4.STGroup;
import org.stringtemplate.v4.STGroupFile;

public class MaudeCompiler extends Main {

    public static STGroup templates;
    static {
        templates = new STGroupFile(MaudeCompiler.class.getResource("/codegen/maude.stg"));
    }

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
        = "main/resources/maude/abs-interpreter.maude";

    SIMULATOR module = SIMULATOR.RL;
    private File outputfile;
    private String mainBlock;
    private int clocklimit = 100;
    private int defaultResources = 0;

    public static void main(final String... args) {
        doMain(args);
    }


    public static int doMain(final String... args) {
        MaudeCompiler compiler = new MaudeCompiler();
        try {
            return compiler.compile(args);
        } catch (NotImplementedYetException e) {
            System.err.println(e.getMessage());
            return 1;
        } catch (Exception e) {
            System.err.println("An error occurred during compilation:\n" + e.getMessage());

            if (compiler.debug) {
                e.printStackTrace();
            }
            return 1;
        }
    }

    @Override
    public List<String> parseArgs(String[] args) throws InternalBackendException {
        List<String> restArgs = super.parseArgs(args);
        List<String> remainingArgs = new ArrayList<>();

        for (int i = 0; i < restArgs.size(); i++) {
            String arg = restArgs.get(i);
            if (arg.equals("-timed")) {
                module = SIMULATOR.EQ_TIMED;
            } else if (arg.equals("-o")) {
                i++;
                if (i == restArgs.size()) {
                    throw new InternalBackendException("Missing output file name after '-o'");
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
    public int compile(String[] args) throws Exception {
        if (verbose) System.out.println("Generating Maude code...");
        final Model model = parse(args);
        // Maude has build-in AwaitAsyncCall support
        model.doAACrewrite = false;
        if (model.hasParserErrors()
            || model.hasErrors()
            || model.hasTypeErrors())
        {
            printErrorMessage();
            return 1;
        }

        PrintStream stream = System.out;
        if (outputfile != null) {
            stream = new PrintStream(outputfile);
        }
        // Are we running in the source directory?
        InputStream is = ClassLoader.getSystemResourceAsStream(RUNTIME_INTERPRETER_PATH);
        if (is == null) {
            // Are we running within absfrontend.jar?
            is = ClassLoader.getSystemResourceAsStream("maude/abs-interpreter.maude");
        }
        if (is == null) {
            throw new InternalBackendException("Could not locate abs-interpreter.maude");
        }
        DateFormat dateFormat = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss");

        stream.println("*** Generated " + dateFormat.format(new Date()));
        ByteStreams.copy(is, stream);
        model.generateMaude(stream, module, mainBlock, clocklimit, defaultResources);
        if (verbose) System.out.println("Finished.  Start `maude " + outputfile.toString() + "' to run the model.");
        return 0;
    }

    public static void printUsage() {
        System.out.println("Maude Backend (-maude):\n"
                + "  -main=<ModuleName> \n"
                + "                 sets the main block to execute\n"
                + "  -o <file>      write output to <file> instead of standard output\n"
                + "  -timed         generate code for timed interpreter\n"
                + "  -limit=n       set clock limit for timed interpreter to n (default 100)\n"
                + "                 (implies -timed)\n"
                + "  -defaultcost=n set default statement execution cost (default 0)\n"
        );
    }

}

// Local Variables:
// tab-width: 4
// End:
