/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.maude;

import java.io.File;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import abs.frontend.ast.Model;
import abs.frontend.parser.Main;

public class MaudeCompiler extends Main {
    
    public static String SIMULATOR_RL = "ABS-SIMULATOR-RL";
    public static String SIMULATOR_EQ_TIMED = "ABS-SIMULATOR-EQ-TIMED";

    String module = SIMULATOR_RL;
    private File outputfile;
    private String mainBlock;
    private int clocklimit = 100;
    private int defaultResources = 0;
    
    public static void main(final String... args) {
        try {
            new MaudeCompiler().compile(args);
        } catch (Exception e) {
            System.err.println("An error occurred during compilation: " + e.getMessage());

            if (Arrays.asList(args).contains("-debug")) {
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
                module = SIMULATOR_EQ_TIMED;
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
                clocklimit = Integer.parseInt(arg.split("=")[1]);
            } else if (arg.startsWith("-defaultcost=")) {
                defaultResources = Integer.parseInt(arg.split("=")[1]);
            }else {
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
        final Model model = parse(args);
        if (model.hasParserErrors() || model.hasErrors() || model.hasTypeErrors())
            return;

        PrintStream stream = System.out;
        if (outputfile != null) {
            stream = new PrintStream(outputfile);
        }
        
        model.generateMaude(stream, module, mainBlock, clocklimit, defaultResources);
    }

    protected void printUsage() {
        super.printUsage();
        System.out.println("Maude Backend:\n"
                + "  -main=<ModuleName> \n" 
                + "                 sets the main block to execute\n"
                + "  -o <file>      write output to <file> instead of standard output\n"
                + "  -timed         generate code for timed interpreter\n"
                + "  -limit=n       set clock limit for timed interpreter to n (default 100)\n"
                + "  -defaultcost=n set default statement execution cost (default 0)\n"
        );
    }

}

// Local Variables:
// tab-width: 4
// End:
