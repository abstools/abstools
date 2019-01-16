/**
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.erlang;

import com.google.common.annotations.VisibleForTesting;
import java.io.BufferedReader;
import java.io.File;
import java.io.FilenameFilter;
import java.io.InputStreamReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.EnumSet;

import org.abs_models.frontend.parser.Main;
import org.apache.commons.io.IOUtils;

import org.abs_models.backend.common.InternalBackendException;
import org.abs_models.common.NotImplementedYetException;
import org.abs_models.frontend.ast.Model;
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
    private File http_index_file = null;
    private File http_static_dir = null;
    private EnumSet<CompileOptions> compileOptions = EnumSet.noneOf(CompileOptions.class);

    public static int minErlangVersion = 19;

    public static void main(final String... args) {
        doMain(args);
    }

    public static int doMain(final String... args) {
        int result = 0;
        ErlangBackend backEnd = new ErlangBackend();
        try {
            result = backEnd.compile(args);
        } catch (InternalBackendException e) {
            System.err.println(e.getMessage());
            return 1;
        } catch (NotImplementedYetException e) {
            System.err.println(e.getMessage());
            return 1;
        } catch (Exception e) {
            System.err.println("An error occurred during compilation:\n" + e.getMessage());
            if (backEnd.debug) {
                e.printStackTrace();
            }
            return 1;
        }
        return result;
    }

    @Override
    public List<String> parseArgs(String[] args) throws InternalBackendException {
        List<String> restArgs = super.parseArgs(args);
        List<String> remainingArgs = new ArrayList<>();

        for (int i = 0; i < restArgs.size(); i++) {
            String arg = restArgs.get(i);
            if (arg.equals("-erlang")) {
                // nothing to do
            } else if (arg.equals("-d")) {
                i++;
                if (i == restArgs.size()) {
                    throw new InternalBackendException("Please provide an output directory");
                } else {
                    destDir = new File(restArgs.get(i));
                }
            } else if(arg.equals("-http-index-file")) {
                i++;
                if (i == restArgs.size()) {
                    throw new InternalBackendException("Please provide an index.html file");
                } else {
                    http_index_file = new File(restArgs.get(i));
                }
            } else if(arg.equals("-http-static-dir")) {
                i++;
                if (i == restArgs.size()) {
                    throw new InternalBackendException("Please provide a directory with static files");
                } else {
                    http_static_dir = new File(restArgs.get(i));
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
                           + "                 Results in <dir>/absmodel/*.gcov after model finishes)\n"
                           + "  -http-index-file <file>\n"
                           + "                 Display <file> when accessing the Model API via browser\n"
                           + "  -http-static-dir <dir>\n"
                           + "                 Make contents of <dir> accessible below /static/ in Model API\n\n"
                           + "  For help on Erlang runtime options, start model with -h\n");
    }

    private int compile(String[] args) throws Exception {
        final Model model = parse(args);
        if (model.hasParserErrors() || model.hasErrors() || model.hasTypeErrors()) {
            printErrorMessage();
            return 1;
        }
        compile(model, destDir, compileOptions);
        return 0;
    }

    @VisibleForTesting
    static boolean isWindows() {
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
        ir.close();
        return version;
    }

    public void compile(Model m, File destDir, EnumSet<CompileOptions> options) throws IOException, InterruptedException, InternalBackendException {
        int version = getErlangVersion();
        if (version < minErlangVersion) {
            String message = "ABS requires at least erlang version " + Integer.toString(minErlangVersion) + ", installed version is " + Integer.toString(version);
            throw new InternalBackendException(message);
        }
        ErlApp erlApp = new ErlApp(destDir, http_index_file, http_static_dir);
        m.generateErlangCode(erlApp, options);
        erlApp.close();

        List<String> compile_command = new ArrayList<String>();
        // We used to call "rebar compile" here but calling erlc directly
        // removes 1.5s from the compile time
        compile_command.add("erlc");
        if (options.contains(CompileOptions.DEBUG)) {
            compile_command.add("+debug_info");
        }
        compile_command.add("-I");
        compile_command.add(destDir + "/absmodel/include");
        compile_command.add("-o");
        compile_command.add(destDir + "/absmodel/ebin");
        Arrays.stream(new File(destDir, "absmodel/src/")
                      .listFiles(new FilenameFilter() {
                              public boolean accept(File dir, String name) {
                                  return name.endsWith(".erl");
                              }}))
            .forEach((File f) -> compile_command.add(f.toString()));
        Process p = Runtime.getRuntime().exec(compile_command.toArray(new String[0]));
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
