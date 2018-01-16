/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.coreabs;

import java.io.PrintStream;
import java.util.ArrayList;
import java.util.List;

import abs.frontend.ast.Model;
import abs.frontend.parser.Main;

public class CoreAbsBackend extends Main {


    public CoreAbsBackend() {
        super();
        // TODO Auto-generated constructor stub
    }

    public void mainMethod(final String... args) {
        try {
            Model m = new CoreAbsBackend().parse(args);
            PrintStream stream = System.out;
            m.generateCoreABS(stream);
            System.exit(0);
        } catch (Exception e) {
            printErrorAndExit(e.getMessage());
        }
    }

    /**
     * @param args
     */
    public static void main(final String... args)  {
        new CoreAbsBackend().mainMethod(args);
    }

    @Override
    public List<String> parseArgs(String[] args) {
        List<String> restArgs = super.parseArgs(args);
        List<String> remainingArgs = new ArrayList<>();

        for (int i = 0; i < restArgs.size(); i++) {
            String arg = restArgs.get(i);
            if (arg.equals("-coreabs")) {
                // nothing to do
            } else {
                remainingArgs.add(arg);
            }
        }
        return remainingArgs;
    }

    public static void printUsage() {
        System.out.println("Core ABS Backend (-coreabs): (no additional options)\n"
        );
    }
}
