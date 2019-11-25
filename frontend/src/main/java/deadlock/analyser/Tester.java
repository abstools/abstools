/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package deadlock.analyser;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.abs_models.Absc;
import org.abs_models.backend.common.InternalBackendException;
import org.abs_models.common.NotImplementedYetException;
import org.abs_models.common.WrongProgramArgumentException;
import org.abs_models.frontend.ast.Model;
import org.abs_models.frontend.delta.DeltaModellingException;
import org.abs_models.frontend.parser.Main;

public class Tester extends Main {

    protected int numberOfIterations = 3;
    private int fixpointVersion = 2;

    public static void main(final String... args) {
        try {
            System.exit(new Tester().compile(args));
        } catch (NotImplementedYetException e) {
            System.err.println(e.getMessage());
            System.exit(127);
        } catch (Exception e) {
            System.err.println("An error occurred during compilation:\n" + e.getMessage());

            if (Arrays.asList(args).contains("-debug")) {
                e.printStackTrace();
            }

            System.exit(127);
        }
    }

    private int compile(String[] args) throws DeltaModellingException, IOException, WrongProgramArgumentException, InternalBackendException {
        // initialize numberOfIterations, fixpointVersion
        List<String> upstreamArgs = this.parseArgs(args);
        Absc absc = Absc.parseArgs(upstreamArgs.toArray(new String[0]));
        this.arguments = absc;
        final Model model = this.parse(arguments.files);
        if (model == null || model.hasParserErrors() || model.hasErrors() || model.hasTypeErrors()) {
	    return 127;
        }

        if (arguments.verbose) {
            System.out.println("Starting deadlock analysis...");
        }
        /*Instantiate the analyzer and perform deadlock analysis*/
        Analyser a = new Analyser();
        return a.deadlockAnalysis(model, arguments.verbose, numberOfIterations, fixpointVersion, System.out);
    }

    @Override
    public List<String> parseArgs(String[] args) throws InternalBackendException {
        List<String> remainingArgs = new ArrayList<>();

        for (int i = 0; i < args.length; i++) {
            String arg = args[i];
            if (arg.equals("-h")) {
                printUsage();
            }
            else if (arg.startsWith("-it=")){
                try{
                       numberOfIterations = Integer.parseInt(arg.split("=")[1]);
                   } catch (Exception e) {
                       System.err.println("The number of iterations (-it) should be an integer");
                       System.exit(1);
                   }
            }
            else if (arg.startsWith("-fixPointVersion=")){
                try{
                    fixpointVersion = Integer.parseInt(arg.split("=")[1]);

                } catch (Exception e) {
                    System.err.println(args.toString());
                    System.err.println(e.toString());
                    System.err.println("The fix point version (-fixPointVersion) should be an integer. Default value 1 for original version, value 2 for the newest version");
                    System.exit(1);
                }
            }
            else {
                remainingArgs.add(arg);
            }
        }
        return remainingArgs;
    }

    public static void printUsage() {
        Main.printUsage();
        System.out.println("Deadlock analyzer:\n"
                + "  -it=<var>     max number of iterations before saturating\n"
                + "  -fixPointVersion=<var>     Default value 1 for original version, value 2 for the newest version\n");
    }

}
