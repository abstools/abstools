/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.scala;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import abs.frontend.ast.Model;
import abs.frontend.parser.Main;

/**
 * Rudimentary core for the entry point to the Scala backend. This class should
 * not be used directly; if you want to use the Scala code generator, please 
 * use the abs-maven-plugin or abs-scala-compiler project.
 * 
 * @author Andri Saar <andri@cs.ioc.ee>
 */
public class ScalaBackend extends Main {
    protected File outputDir = null;
    protected boolean sourceOnly = true;

    protected void compileSources() {
        throw new UnsupportedOperationException();
    }
    
    final int compile(String[] args) throws Exception {
        final Model model = parse(args);
        
        if (model.hasParserErrors() || model.hasErrors() || model.hasTypeErrors())
            return -1;
        
        if (!outputDir.exists()) {
            System.err.println("Destination directory " + outputDir.getAbsolutePath() + " does not exist!");
            return -1;
        }

        if (!outputDir.canWrite()) {
            System.err.println("Destination directory " + outputDir.getAbsolutePath() + " cannot be written to!");
            return -1;
        }

        model.generateScala(outputDir);
        
        if (!sourceOnly)
            compileSources();
        
        return 0;
    }
    
    @Override
    public List<String> parseArgs(String[] args) throws Exception {
        List<String> restArgs = super.parseArgs(args);
        List<String> remainingArgs = new ArrayList<String>();

        for (int i = 0; i < restArgs.size(); i++) {
            String arg = restArgs.get(i);
            if (arg.equals("-d")) {
                i++;
                if (i == restArgs.size()) {
                    System.err.println("Please provide a destination directory");
                    System.exit(1);
                } else {
                    outputDir = new File(args[i]);
                }
            } else if (arg.equals("-sourceonly")) {
                this.sourceOnly = true;
            } else {
                remainingArgs.add(arg);
            }
        }

        return remainingArgs;
    }

    @Override
    protected void printUsage() {
        super.printUsage();
        System.out.println("Scala backend:");
        System.out.println("  -d <dir>       outputs generated files to given directory");
        System.out.println("  -sourceonly    do not compile generated class files");
    }
    
    public static void main(final String... args) {
        try {
            new ScalaBackend().compile(args);
        } catch (Exception e) {
            System.err.println("An error occurred during compilation: " + e.getMessage());

            if (Arrays.asList(args).contains("-debug")) {
                e.printStackTrace();
            }

            System.exit(1);
        }
    }
}
