/**
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.erlang;

import java.io.BufferedReader;
import java.io.File;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.EnumSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import com.google.common.annotations.VisibleForTesting;

import org.abs_models.Absc;
import org.abs_models.backend.common.InternalBackendException;
import org.abs_models.common.NotImplementedYetException;
import org.abs_models.frontend.ast.Model;
import org.abs_models.frontend.parser.Main;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.io.IOUtils;
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

    public final static int minErlangVersion = 22;

    public static int doMain(Absc args) {
        int result = 0;
        ErlangBackend backEnd = new ErlangBackend();
        backEnd.arguments = args;
        try {
            result = backEnd.compile();
        } catch (InternalBackendException e) {
            System.err.println(e.getMessage());
            return 1;
        } catch (NotImplementedYetException e) {
            System.err.println(e.getMessage());
            return 1;
        } catch (Exception e) {
            System.err.println("An error occurred during compilation:\n" + e.getMessage());
            if (backEnd.arguments.debug) {
                e.printStackTrace();
            }
            return 1;
        }
        return result;
    }

    private int compile() throws Exception {
        final Model model = parse(arguments.files);
        if (model.hasParserErrors() || model.hasErrors() || model.hasTypeErrors()) {
            printErrorMessage();
            return 1;
        }
        File outdir = arguments.destDir;
        if (outdir.getPath().equals("gen")) {
            // KLUDGE: "gen/" is the default path for java and erlang;
            // keep old erlang behavior of defaulting to "gen/erl/".
            // Note that we can't generate directly into "gen/" since
            // we don't know if the user explicitly specified "gen/"
            // or didn't say anything about the output directory
            outdir = new File("gen/erl/");
        }
        EnumSet<CompileOptions> compileOptions = EnumSet.noneOf(CompileOptions.class);
        if (arguments.verbose) compileOptions.add(CompileOptions.VERBOSE);
        if (arguments.debug) compileOptions.add(CompileOptions.DEBUG);
        if (arguments.debug_generated_code) compileOptions.add(CompileOptions.COVERAGE);
        compile(model, outdir, compileOptions);
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
        String ebin_dir = "/absmodel/_build/default/lib/absmodel/ebin/";
        ErlApp erlApp = new ErlApp(destDir, arguments.http_index_file, arguments.http_static_dir);
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
        compile_command.add(destDir + ebin_dir);
        // Set<String> compiled_basenames =
        //     Arrays.stream(new File(destDir, ebin_dir).listFiles())
        //     .map((File f) -> FilenameUtils.removeExtension(f.getName()))
        //     .collect(Collectors.toSet());
        Arrays.stream(new File(destDir, "absmodel/src/")
                      .listFiles(new FilenameFilter() {
                              public boolean accept(File dir, String name) {
                                  return name.endsWith(".erl")
                                      // && !compiled_basenames.contains(FilenameUtils.removeExtension(name))
                                      ;
                              }
                          }))
            .forEach((File f) -> compile_command.add(f.toString()));

        if (options.contains(CompileOptions.VERBOSE)) {
            System.out.println("Compiling erlang files with command: "
                               + String.join(" ", compile_command));
        }

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
