package org.abs_models;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.stream.Collectors;

import org.abs_models.frontend.parser.Main;
import org.abs_models.frontend.typechecker.locationtypes.LocationType;
import org.abs_models.frontend.typechecker.locationtypes.infer.LocationTypeInferrerExtension;
import org.abs_models.frontend.typechecker.locationtypes.infer.LocationTypeInferrerExtension.LocationTypingPrecision;

import picocli.CommandLine;
import picocli.CommandLine.Command;
import picocli.CommandLine.ITypeConverter;
import picocli.CommandLine.IVersionProvider;
import picocli.CommandLine.Option;
import picocli.CommandLine.Parameters;

/**
 * The main entry point for absc, the abs compiler.
 *
 * This class contains command line parsing and carries command line
 * options as public fields.
 */
@Command(name = "absc",
         description = "Check and compile abs models",
         header = {
             "    Copyright (c) 2009-2013, The HATS Consortium",
             "    Copyright (c) 2013-2016, The Envisage Project",
             "    Copyright (c) 2016-2019, SIRIUS Center",
             "    http://www.abs-models.org/" },
         mixinStandardHelpOptions = true, // handles -h, -V
         //abbreviateSynopsis = true,
         customSynopsis = "@|bold absc|@ [BACKEND] [OPTIONS] [<files>...]",
         sortOptions = false,
         separator = " ",       // -product=XXX vs. -o file
         versionProvider = Absc.AbscVersionProvider.class
         )
public class Absc implements Callable<Void> {

    @Parameters(description = "ABS files/directories/packages to handle",
                arity = "1..*")
    public List<File> files;

    @Option(names = { "--erlang", "-erlang" },
            description = "@|bold Erlang backend:|@ generate Erlang code")
    public boolean erlang = false;
    @Option(names = { "--maude", "-maude" },
            description = "@|bold Maude backend:|@ generate Maude code")
    public boolean maude = false;
    @Option(names = { "--java", "-java" },
            description = "@|bold Java backend:|@ generate Java code")
    public boolean java = false;
    @Option(names = { "--prolog", "-prolog" },
            description = "@|bold Prolog backend:|@ generate Prolog data file")
    public boolean prolog = false;

    @Option(names = { "--prettyprint", "-prettyprint" },
            description = "@|bold Pretty-printer:|@ pretty print model and exit")
    public boolean prettyprint = false;
    @Option(names = { "--coreabs", "-coreabs" },
            description = "generate Coreabs data file (XXX undocumented in previous version)")
    public boolean coreabs = false;
    @Option(names = { "--outline", "-outline" },
            description = "generate code structure outline")
    public boolean outline = false;

    @Option(names = { "--dump-products" },
            description = "print all defined products in one line")
    public boolean dumpProducts = false;


    @Option(names = { "-v", "--verbose" },
            description = "verbose output")
    public boolean verbose = false;
    @Option(names = { "-debug"},
            description = "print diagnostic information (e.g., stacktraces) for internal compiler problems")
    public boolean debug = false;
    @Option(names = { "-dump"},
            description = "dump AST to standard output")
    public boolean dump = false;

    // Code generation options
    @Option(names = { "-o"},
            description = "for single-file backends: compile to @|italic file|@ (default: standard output)",
            paramLabel = "file")
    public File outputfile;
    @Option(names = { "-d"},
            description = "compile to @|italic directory|@ (default: @|bold gen/|@ for multi-file backends, ignored for single-file backends)",
            paramLabel = "directory")
    public File destDir = new File("gen/");

    // Erlang options
    @Option(names = { "-cover" },
            description = "@|bold Erlang backend:|@ compile with run-time statement execution count recording")
    public boolean erlang_cover = false;
    @Option(names = { "-http-index-file" },
            description = "@|bold Erlang backend:|@ display @|italic file|@ when accessing the Model API with a web browser",
            paramLabel = "file")
    public File http_index_file;
    @Option(names = { "-http-static-dir" },
            description = "@|bold Erlang backend:|@ make contents of @|italic dir|@ accessible below @|bold /static/|@ in Model API",
            paramLabel = "dir")
    public File http_static_dir;

    // Maude options
    @Option(names = { "-main" },
            description = "@|bold Maude backend:|@ select the main block to execute",
            paramLabel = "module name")
            public String maude_mainBlock;
    @Option(names = { "-timed"},
            description = "@|bold Maude backend:|@ generate code for timed interpreter")
    public boolean maude_timed = false;
    @Option(names = { "-limit"},
            description = "@|bold Maude backend:|@ set clock limit for timed interpreter to @|italic n|@ (default: ${DEFAULT-VALUE})",
            paramLabel = "n")
    public int maude_clocklimit = 100;
    @Option(names = { "-defaultcost"},
            description = "@|bold Maude backend:|@ set default statement execution cost to @|italic n|@ (default: ${DEFAULT-VALUE})",
            paramLabel = "n")
    public int maude_defaultResources = 0;

    // Java options
    // -sourceonly omitted, -dynamic does not work
    @Option(names = { "-sourceonly" },
            description = "@|bold Java backend:|@ only generate Java source files")
    public boolean java_sourceOnly = false;
    @Option(names = { "-debuginfo" },
            description = "@|bold Java backend:|@ generate code with listener / debugger support (increases code size)")
    public boolean java_includeDebug = false;

    // Pretty-printer
    @Option(names = { "-f" },
            description = "@|bold Pretty-printer, Outline:|@ ignore errors in model")
    public boolean prettyprint_force = false;
    @Option(names = { "-keepsugar" },
            description = "@|bold Pretty-printer: |@ do not transform statements into basic core abs")
    public boolean prettyprint_keepsugar = false;
    @Option(names = { "-keepstdlib" },
            description = "@|bold Pretty-printer: |@ include ABS standard library")
    public boolean prettyprint_keepstdlib = false;
    // SPL
    @Option(names = { "-product" },
            description = "apply the deltas specified by @|italic product|@")
    public String product = null;

    @Option(names = { "-checkspl" },
            description = "check the SPL for errors")
    public boolean checkspl = false;

    @Option(names = { "-notypecheck" },
            description = "disable typechecking")
    public boolean notypecheck = false;

    @Option(names = { "-loctypes" },
            description = "enable location type checking")
    public boolean locationTypeInferenceEnabled = false;

    static class LocationTypeUserTypes extends ArrayList<String> {
        LocationTypeUserTypes() {
            super(Arrays.stream(LocationType.ALLUSERTYPES)
                  .map(LocationType::toString)
                  .collect(Collectors.toList()));
        }
    }
    static class LocationTypeConverter implements ITypeConverter<LocationType> {
        public LocationType convert(String value) throws Exception {
            return LocationType.createFromName(value);
        }
    }
    @Option(names = { "-locdefault" },
            description = "sets the default location type (allowed values: ${COMPLETION-CANDIDATES}) (default: ${DEFAULT-VALUE})",
            completionCandidates = LocationTypeUserTypes.class,
            converter = LocationTypeConverter.class,
            paramLabel = "loctype")
    // default value taken from declaration of
    // LocationTypeInferrerExtension.defaultType
    public LocationType defaultLocationType = LocationType.INFER;


    @Option(names = { "-locscope" },
            description = "sets the location aliasing scope (allowed values: ${COMPLETION-CANDIDATES}) (default: ${DEFAULT-VALUE})",
            paramLabel = "scope")
    // default value taken from declaration of
    // LocationTypeInferrerExtension.precision
    public LocationTypeInferrerExtension.LocationTypingPrecision locationTypeScope = LocationTypingPrecision.CLASS_LOCAL_FAR;

    // mTVL options
    @Option(names = { "-solve" },
            description = "solve constraint satisfaction problem (CSP) for the feature model and print a solution")
    public boolean solve = false ;
    @Option(names = { "-solveall" },
            description = "solve all solutions for the CSP and print timing information")
    public boolean solveall = false ;
    @Option(names = { "-solveWith" },
            description = "solve CSP by finding a product that includes @|italic PID|@",
            paramLabel = "PID")
    public String solveWithProduct ;
    @Option(names = { "-min" },
            description = "minimise variable @|italic var|@ when solving the CSP for the feature model",
            paramLabel = "var")
    public String minimise;
    @Option(names = { "-max" },
            description = "maximise variable @|italic var|@ when solving the CSP for the feature model",
            paramLabel = "var")
    public String maximise;
    @Option(names = { "-maxProduct" },
            description = "print the solution that has the most number of features")
    public boolean maxProduct = false ;
    @Option(names = { "-minWith" },
            description = "solve CSP by finding a solution that tries to include @|italic product|@ with minimum number of changes",
            paramLabel = "product")
    public String minWith;
    @Option(names = { "-nsol" },
            description = "count the number of solutions")
    public boolean numbersol = false ;
    @Option(names = { "-noattr" },
            description = "ignore attributes when generating product")
    public boolean ignoreattr = false ;
    @Option(names = { "-check" },
            description = "check satisfiability of @|italic product|@",
            paramLabel = "product")
    public String checkProduct;

    static class AbscVersionProvider implements IVersionProvider {
        public String[] getVersion() throws Exception {
            String version = Absc.class.getPackage().getImplementationVersion();
            String gitversion = Absc.class.getPackage().getSpecificationVersion();
            if (version == null)
                version = "HEAD";
            if (gitversion == null)
                gitversion = "HEAD-dirty";
            return new String[] {
                "ABS Tool Suite version " + version,
                "Built from git tree " + gitversion
            };
        }
    }

    /**
     * Parse a command-line string as given by a user. Returns a
     * populated instance for use by legacy entry points which are not
     * dispatched via {@link org.abs_models.frontend.parser.Main Main}.
     * @param args the command line options
     * @return an initialized Absc instance
     */
    public static Absc parseArgs(String[] args) {
        return CommandLine.populateCommand(new Absc(), args);
    }

    /**
     * The main entry point for `absc`.  Command line parsing happens
     * in this class, type-checking and dispatching to backends are
     * done by {@link org.abs_models.frontend.parser.Main Main}.
     * @param args
     */
    public static void main(String[] args) {
        CommandLine.call(new Absc(), args);
    }

    @Override
    public Void call() throws Exception {
        Main main = new Main();
        int result = main.mainMethod(this);
        if (result != 0) System.exit(result);
        return null;
    }
}
