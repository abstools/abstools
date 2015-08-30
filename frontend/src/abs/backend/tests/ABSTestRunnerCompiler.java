/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.tests;

import java.io.File;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import abs.common.NotImplementedYetException;
import abs.frontend.ast.Model;
import abs.frontend.parser.Main;

/**
 * 
 * @author pwong
 *
 */
public class ABSTestRunnerCompiler extends Main {
    private File outputfile;
    
    public static void main(final String... args) {
        try {
            new ABSTestRunnerCompiler().compile(args);
        } catch (NotImplementedYetException e) {
            System.err.println(e.getMessage());
            System.exit(0);
        } catch (Exception e) {
            System.err.println("An error occurred during compilation:\n" + e.getMessage());

            // FIXME: switch doesn't work.
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
        final Model model = parse(args);
        if (model.hasParserErrors() || model.hasErrors() || model.hasTypeErrors())
            return;

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
            System.out.println("Generating Test Runner to "+loc+"...");
        }
        
        ABSTestRunnerGenerator gen = new ASTBasedABSTestRunnerGenerator(model);
        if (gen.hasUnitTest()) {
            gen.generateTestRunner(stream);
        } else {
            throw new IllegalStateException("Cannot generate test runner");
        }
    }
    
    protected void printUsage() {
        super.printUsage();
        System.out.println("ABSUnit Test Runner Generator:\n"
                + "  -o <file>      write output to <file> instead of standard output\n"
        );
    }

}
