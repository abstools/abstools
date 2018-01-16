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
import java.util.EnumSet;

import org.apache.commons.io.IOUtils;

import abs.backend.common.InternalBackendException;
import abs.common.NotImplementedYetException;
import abs.frontend.ast.Model;
import abs.frontend.parser.Main;
import org.apache.commons.io.output.NullOutputStream;

/**
 * Translates given ABS Files to an Erlang program
 *
 * @author Georg Göri
 *
 */
public class ErlangBackend extends Main {

    // Compile option flags.  TODO: this could be moved to Main and unified with the other backends.
    public enum CompileOptions {
        DEBUG,
        VERBOSE,
        COVERAGE
    }

    private File destDir = new File("gen/erl/");
    private EnumSet<CompileOptions> compileOptions = EnumSet.noneOf(CompileOptions.class);

    public static int minErlangVersion = 19;

    public static void main(final String... args) {
        ErlangBackend backEnd = new ErlangBackend();
        try {
            backEnd.compile(args);
        } catch (InternalBackendException e) {
            System.err.println(e.getMessage());
            System.exit(1);
        } catch (NotImplementedYetException e) {
            System.err.println(e.getMessage());
            System.exit(0);
        } catch (Exception e) {
            System.err.println("An error occurred during compilation:\n" + e.getMessage());
            if (backEnd.debug) {
                e.printStackTrace();
            }
            System.exit(1);
        }
    }

    @Override
    public List<String> parseArgs(String[] args) {
        List<String> restArgs = super.parseArgs(args);
        List<String> remainingArgs = new ArrayList<>();

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
            } else if (arg.equals("-cover")) {
                compileOptions.add(CompileOptions.COVERAGE);
            } else {
                remainingArgs.add(arg);
            }
        }
        if (verbose) compileOptions.add(CompileOptions.VERBOSE); // KLUDGE
        if (debug) compileOptions.add(CompileOptions.DEBUG); // KLUDGE
        return remainingArgs;
    }

    public static void printUsage() {
        System.out.println("Erlang Backend (-erlang):\n"
                           + "  -d <dir>       Create code below <dir> (default gen/erl/)\n"
                           + "  -cover         Compile with run-time statement execution count recording.\n"
                           + "                 Results in <dir>/absmodel/*.gcov after model finishes)\n\n"
                           + "  For help on Erlang runtime options, start model with -h\n");
    }

    private void compile(String[] args) throws Exception {
        final Model model = parse(args);
        if (model.hasParserErrors() || model.hasErrors() || model.hasTypeErrors())
            printParserErrorAndExit();
        compile(model, destDir, compileOptions);
    }

    private static boolean isWindows() {
        return System.getProperty("os.name").toLowerCase().contains("win");
    }

    private static String getEscapedDoubleQuote() {
        if(isWindows()) {
            // "" will be resolved to "
            return "\"\"";
        } else {
            // " just works
            return "\"";
        }
    }

    public static int getErlangVersion()  throws IOException, InterruptedException {
        // check erlang version number
        String erlVersionCheckCode = "io:fwrite("
            + getEscapedDoubleQuote()
            + "~s\n"
            + getEscapedDoubleQuote()
            + ", [erlang:system_info(otp_release)]), halt().";
        Process versionCheck = Runtime.getRuntime().exec(new String[] { "erl", "-eval", erlVersionCheckCode, "-noshell" });
        versionCheck.waitFor();
        BufferedReader ir = new BufferedReader(new InputStreamReader(versionCheck.getInputStream()));
        int version = Integer.parseInt(ir.readLine());
        return version;
    }

    public static void compile(Model m, File destDir, EnumSet<CompileOptions> options) throws IOException, InterruptedException, InternalBackendException {
        int version = getErlangVersion();
        if (version < minErlangVersion) {
            String message = "ABS requires at least erlang version " + Integer.toString(minErlangVersion) + ", installed version is " + Integer.toString(version);
            throw new InternalBackendException(message);
        }
        ErlApp erlApp = new ErlApp(destDir);
        m.generateErlangCode(erlApp, options);
        erlApp.close();
	String[] rebarProgram = new String[] {"escript", "../bin/rebar", "compile"};
        Process p = Runtime.getRuntime().exec(rebarProgram, null, new File(destDir, "absmodel"));
        if (options.contains(CompileOptions.VERBOSE)) IOUtils.copy(p.getInputStream(), System.out);
        else IOUtils.copy(p.getInputStream(), new NullOutputStream());
        p.waitFor();
        if (p.exitValue() != 0) {
            String message = "Compilation of generated erlang code failed with exit value " + p.exitValue();
            if (!options.contains(CompileOptions.VERBOSE)) message = message + "\n  (use -v for detailed compiler output)";
            throw new InternalBackendException(message);
            // TODO: consider removing the generated code here.  For now,
            // let's leave it in place for diagnosis.
        } else {
            if (options.contains(CompileOptions.VERBOSE)) {
                System.out.println();
                System.out.println("Finished.  \"gen/erl/run\" to start the model.");
                System.out.println("          (\"gen/erl/run --help\" for more options)");
            }
        }
    }

}
