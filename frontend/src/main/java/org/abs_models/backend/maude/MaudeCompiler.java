/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.maude;

import java.io.InputStream;
import java.io.PrintStream;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import com.google.common.io.ByteStreams;

import org.abs_models.Absc;
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
    private String mainBlock;
    private int clocklimit = 100;
    private int defaultResources = 0;

    public static int doMain(Absc args) {
        MaudeCompiler compiler = new MaudeCompiler();
        compiler.arguments = args;
        try {
            return compiler.compile();
        } catch (NotImplementedYetException e) {
            System.err.println(e.getMessage());
            return 1;
        } catch (Exception e) {
            System.err.println("An error occurred during compilation:\n" + e.getMessage());

            if (compiler.arguments.debug) {
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
     * @throws Exception
     */
    public int compile() throws Exception {
        if (arguments.verbose) System.out.println("Generating Maude code...");
        module = arguments.maude_timed ? SIMULATOR.EQ_TIMED : SIMULATOR.RL;
        final Model model = parse(arguments.files);
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
        if (arguments.outputfile != null) {
            stream = new PrintStream(arguments.outputfile);
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
        model.generateMaude(stream, module, arguments.maude_mainBlock, arguments.maude_clocklimit, arguments.maude_defaultResources);
        if (arguments.verbose && arguments.outputfile != null) {
            System.out.println("Finished.  Start `maude " + arguments.outputfile.toString() + "' to run the model.");
        }
        return 0;
    }
}

// Local Variables:
// tab-width: 4
// End:
