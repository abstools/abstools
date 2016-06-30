/** 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.erlang;

import java.io.BufferedReader;
import java.io.File;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.apache.commons.io.IOUtils;

import abs.backend.common.InternalBackendException;
import abs.common.NotImplementedYetException;
import abs.frontend.ast.Model;
import abs.frontend.parser.Main;

/**
 * Translates given ABS Files to an Erlang program
 * 
 * @author Georg Göri
 * 
 */
public class ErlangBackend extends Main {

    private File destDir = new File("gen/erl/");

    private static int minVersion = 18;

    public static void main(final String... args) {
        try {
            new ErlangBackend().compile(args);
        } catch (InternalBackendException e) {
            System.err.println(e.getMessage());
            System.exit(1);
        } catch (NotImplementedYetException e) {
            System.err.println(e.getMessage());
            System.exit(0);
        } catch (Exception e) {
            System.err.println("An error occurred during compilation:\n" + e.getMessage());
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
            if (arg.equals("-erlang")) {
                // nothing to do
            } else if (arg.equals("-d")) {
                i++;
                if (i == restArgs.size()) {
                    System.err.println("Please provide an output directory");
                    System.exit(1);
                } else {
                    destDir = new File(restArgs.get(i));
                }
            } else {
                remainingArgs.add(arg);
            }
        }

        return remainingArgs;
    }

    @Override
    protected void printUsage() {
        super.printUsage();
        System.out.println("Erlang Backend:\n"
                           + "  -d <dir>       Create code below <dir> (default gen/erl/)\n");
    }

    private void compile(String[] args) throws Exception {
        final Model model = parse(args);
        if (model.hasParserErrors() || model.hasErrors() || model.hasTypeErrors())
            printParserErrorAndExit();
        compile(model, destDir, verbose);
    }

    public static void compile(Model m, File destDir, boolean verbose) throws IOException, InterruptedException, InternalBackendException {
        if (verbose) System.out.println("Generating Erlang code...");

        // check erlang version number
        Process versionCheck = Runtime.getRuntime().exec(new String[] { "erl", "-eval", "io:fwrite(\"~s\n\", [erlang:system_info(otp_release)]), halt().", "-noshell" });
        versionCheck.waitFor();
        BufferedReader ir = new BufferedReader(new InputStreamReader(versionCheck.getInputStream()));
        int version = Integer.parseInt(ir.readLine());
        if (version < minVersion) {
            String message = "ABS requires at least erlang version " + Integer.toString(minVersion) + ", installed version is " + Integer.toString(version);
            throw new InternalBackendException(message);
        }
        ErlApp erlApp = new ErlApp(destDir);
        m.generateErlangCode(erlApp);
        erlApp.close();
	String[] rebarProgram = new String[] {"escript", "../bin/rebar", "compile"};
        Process p = Runtime.getRuntime().exec(rebarProgram, null, new File(destDir, "absmodel"));
        if (verbose) IOUtils.copy(p.getInputStream(), System.out);
        p.waitFor();
        if (p.exitValue() != 0) {
            String message = "Compilation of generated erlang code failed with exit value " + p.exitValue();
            if (!verbose) message = message + "\n  (use -v for detailed compiler output)";
            throw new InternalBackendException(message);
            // TODO: consider removing the generated code here.  For now,
            // let's leave it in place for diagnosis.
        } else {
            if (verbose) {
                System.out.println();
                System.out.println("Finished.  \"gen/erl/run\" to start the model.");
                System.out.println("          (\"gen/erl/run --help\" for more options)");
            }
        }
    }

}
