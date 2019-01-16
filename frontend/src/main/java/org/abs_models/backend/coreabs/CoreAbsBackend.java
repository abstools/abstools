/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.coreabs;

import java.io.PrintStream;
import java.util.ArrayList;
import java.util.List;

import org.abs_models.backend.common.InternalBackendException;
import org.abs_models.frontend.ast.Model;
import org.abs_models.frontend.parser.Main;

public class CoreAbsBackend extends Main {


    public CoreAbsBackend() {
        super();
        // TODO Auto-generated constructor stub
    }

    public int mainMethod(final String... args) {
        try {
            Model m = new CoreAbsBackend().parse(args);
            PrintStream stream = System.out;
            m.generateCoreABS(stream);
            return 0;
        } catch (Exception e) {
            printError(e.getMessage());
            return 1;
        }
    }

    public static void main(final String... args) {
        doMain(args);
    }

    public static int doMain(final String... args)  {
        return new CoreAbsBackend().mainMethod(args);
    }

    @Override
    public List<String> parseArgs(String[] args) throws InternalBackendException {
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
