/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.tests;

import java.io.File;
import java.io.PrintStream;
import java.util.Arrays;

import org.abs_models.Absc;
import org.abs_models.common.NotImplementedYetException;
import org.abs_models.frontend.ast.Model;
import org.abs_models.frontend.parser.Main;

/**
 *
 * @author pwong
 *
 */
public class ABSTestRunnerCompiler extends Main {
    private File outputfile;

    public static void main(final String... args) {
        try {
            Absc arguments = Absc.parseArgs(args);
            new ABSTestRunnerCompiler().compile(arguments);
        } catch (NotImplementedYetException e) {
            System.err.println(e.getMessage());
            System.exit(1);
        } catch (Exception e) {
            System.err.println("An error occurred during compilation:\n" + e.getMessage());

            // FIXME: switch doesn't work.
            if (Arrays.asList(args).contains("-debug")) {
                e.printStackTrace();
            }

            System.exit(1);
        }
    }

    /**
     * @param args
     * @throws Exception
     */
    public void compile(Absc args) throws Exception {
        this.arguments = args;
        final Model model = parse(arguments.files);
        if (model.hasParserErrors() || model.hasErrors() || model.hasTypeErrors())
            return;

        final PrintStream stream;
        final String loc;
        if (arguments.outputfile != null) {
            stream = new PrintStream(arguments.outputfile);
            loc = arguments.outputfile.getAbsolutePath();
        } else {
            stream = System.out;
            loc = "Standard Output Stream";
        }

        if (arguments.verbose) {
            System.out.println("Generating Test Runner to "+loc+"...");
        }

        ABSTestRunnerGenerator gen = new ASTBasedABSTestRunnerGenerator(model);
        if (gen.hasUnitTest()) {
            gen.generateTestRunner(stream);
        } else {
            throw new IllegalStateException("Cannot generate test runner");
        }
    }

}
