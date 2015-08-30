/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.keyabs;
import abs.backend.common.CodeStream;
import abs.common.NotImplementedYetException;

import java.io.File;
import java.io.InputStream;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.ArrayList;
import java.util.List;

import com.google.common.io.ByteStreams;

import abs.frontend.ast.Model;
import abs.frontend.parser.Main;

public class KeyAbsBackend extends Main {

    private File outputfile;
    private static boolean debug = false;
    
    public static void main(final String... args) {
        /* Maude has build-in AwaitAsyncCall support */
        Model.doAACrewrite = false;
        try {
            new KeyAbsBackend().compile(args);
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
            if (arg.equals("-o")) {
                i++;
                if (i == restArgs.size()) {
                    System.err.println("Please provide an output file");
                    System.exit(1);
                } else {
                    outputfile = new File(restArgs.get(i));
                }
            } else if (arg.startsWith("-debug")) {
                debug  = true;
            } else if (arg.equals("-keyabs")) {
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
        final Model model = parse(args);
        if (model.hasParserErrors()
            || model.hasErrors()
            || model.hasTypeErrors())
            printParserErrorAndExit();

        CodeStream stream;
        if (outputfile != null) {
            stream = new CodeStream(outputfile);
        } else {
            stream = new CodeStream(System.out, "");
        }
        DateFormat dateFormat = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss");
        stream.println("/* Generated " + dateFormat.format(new Date()) + " */");
        model.generateKeyAbs(stream);
    }

    protected void printUsage() {
        super.printUsage();
        System.out.println("KeyAbs Backend:\n"
                + "  -o <file>      write output to <file> instead of standard output\n"
                + "  -debug         print stacktrace on exception\n"
        );
    }

}

// Local Variables:
// tab-width: 4
// End:
