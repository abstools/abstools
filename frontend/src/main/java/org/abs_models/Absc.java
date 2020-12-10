package org.abs_models;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.stream.Collectors;

import org.abs_models.frontend.parser.Main;
import org.abs_models.frontend.typechecker.CheckSPLCommand;
import org.abs_models.frontend.typechecker.locationtypes.LocationType;

import picocli.CommandLine;
import picocli.CommandLine.ArgGroup;
import picocli.CommandLine.Command;
import picocli.CommandLine.ITypeConverter;
import picocli.CommandLine.IVersionProvider;
import picocli.CommandLine.Option;
import picocli.CommandLine.Parameters;
import picocli.CommandLine.ScopeType;

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
         synopsisHeading = "",
         customSynopsis =  {"Usage: @|bold absc|@ [BACKEND] [OPTIONS] [<files>...]",
                            "   or: @|bold absc|@ checkspl [OPTIONS] [<files>...]"
         },
         sortOptions = false,
         // separator = " ", // no need; " " and "=" are separators by default
         subcommands = {
             // HelpCommand.class, // no need; we have the standard -h / --help options
             CheckSPLCommand.class
         },
         versionProvider = Absc.AbscVersionProvider.class
         )
public class Absc implements Callable<Integer> {

    @Parameters(description = "ABS files/directories/packages to handle",
                arity = "1..*")
    public List<File> files;

    @ArgGroup(exclusive = true)
    public Backend backend;

    public static class Backend {
        @Option(names = { "--erlang", "-e" }, required = true,
                description = "@|bold Erlang backend:|@ generate Erlang code")
        public boolean erlang = false;
        @Option(names = { "--maude", "-m" }, required = true,
                description = "@|bold Maude backend:|@ generate Maude code")
        public boolean maude = false;
        @Option(names = { "--java", "-j" }, required = true,
                description = "@|bold Java backend:|@ generate Java code")
        public boolean java = false;
        @Option(names = { "--prolog" }, required = true,
                description = "@|bold Prolog backend:|@ generate Prolog data file")
        public boolean prolog = false;
        @Option(names = { "--c" }, required = true,
            description = "@|bold C backend:|@ generate C code")
        public boolean c = false;

        @Option(names = { "--prettyprint" }, required = true,
                description = "@|bold Pretty-printer:|@ pretty print model and exit")
        public boolean prettyprint = false;
        @Option(names = { "--coreabs" }, required = true,
                hidden = true,
                description = {"generate Coreabs data file",
                               "undocumented and doesn't do much -- kept around for backward compatibility only"})
        public boolean coreabs = false;
        @Option(names = { "--json" }, required = true,
                description = {"generate JSON data file for autodeployer",
                               "See https://github.com/jacopoMauro/abs_deployer"})
        public boolean json = false;
        @Option(names = { "--outline" }, required = true,
                hidden = true,
                description = {"generate code structure outline for collaboratory",
                               "(not generally useful so we don't advertise it in help output)"})
        public boolean outline = false;
        @Option(names = { "--dump-products" }, required = true,
                description = "print all defined products in one line")
        public boolean dumpProducts = false;
    }


    @Option(names = { "-v", "--verbose" },
            scope = ScopeType.INHERIT,
            description = "verbose output")
    public boolean verbose = false;
    @Option(names = { "--debug"},
            scope = ScopeType.INHERIT,
            description = "print diagnostic information (e.g., stacktraces) for internal compiler problems")
    public boolean debug = false;
    @Option(names = { "--dump"},
            description = "dump AST to standard output")
    public boolean dump = false;

    // Code generation options
    @Option(names = { "-o", "--output-file"},
            description = "for single-file backends: compile to @|italic file|@ (default: standard output)",
            paramLabel = "file")
    public File outputfile;
    @Option(names = { "-d", "--directory"},
            description = "compile to @|italic directory|@ (default: @|bold gen/|@ for multi-file backends, ignored for single-file backends)",
            paramLabel = "directory")
    public File destDir = new File("gen/");
    @Option(names = { "--debuginfo" },
            description = {"@|bold Java backend:|@ generate code with listener / debugger support (increases code size)",
                           "@|bold Erlang backend:|@ generate code with execution coverage recording"})
    public boolean debug_generated_code = false;

    // Erlang options
    @Option(names = { "--modelapi-index-file" },
            description = "@|bold Erlang backend:|@ display @|italic file|@ when accessing the Model API with a web browser",
            paramLabel = "file")
    public File http_index_file;
    @Option(names = { "--modelapi-static-dir" },
            description = "@|bold Erlang backend:|@ make contents of @|italic dir|@ accessible below @|bold /static/|@ in Model API",
            paramLabel = "dir")
    public File http_static_dir;

    // Maude options
    @Option(names = { "--main" },
            description = "@|bold Maude backend:|@ select the main block to execute",
            paramLabel = "module name")
            public String maude_mainBlock;
    @Option(names = { "--timed"},
            description = "@|bold Maude backend:|@ generate code for timed interpreter")
    public boolean maude_timed = false;
    @Option(names = { "--limit"},
            description = "@|bold Maude backend:|@ set clock limit for timed interpreter to @|italic n|@ (default: ${DEFAULT-VALUE})",
            paramLabel = "n")
    public int maude_clocklimit = 100;
    @Option(names = { "--defaultcost"},
            description = "@|bold Maude backend:|@ set default statement execution cost to @|italic n|@ (default: ${DEFAULT-VALUE})",
            paramLabel = "n")
    public int maude_defaultResources = 0;

    // Java options
    // -dynamic does not work, so drop option to try to generate it
    @Option(names = { "--sourceonly" },
            description = "@|bold Java backend:|@ do not generate Java .class files")
    public boolean java_sourceOnly = false;

    // Pretty-printer
    @Option(names = { "-f", "--force" },
            description = "@|bold Pretty-printer, Outline:|@ ignore errors in model")
    public boolean prettyprint_force = false;
    @Option(names = { "--keepsugar" },
            description = "@|bold Pretty-printer: |@ do not transform statements into basic core abs")
    public boolean prettyprint_keepsugar = false;
    @Option(names = { "--keepstdlib" },
            description = "@|bold Pretty-printer: |@ include ABS standard library")
    public boolean prettyprint_keepstdlib = false;
    // SPL
    @Option(names = { "-p", "--product" },
            description = "apply the deltas specified by @|italic product|@")
    public String product = null;

    @Option(names = { "--notypecheck" },
            description = "disable typechecking")
    public boolean notypecheck = false;

    @Option(names = { "--loctypecheck" },
            description = "enable location type checking")
    public boolean locationTypeInferenceEnabled = false;

    static class LocationTypeUserTypes extends ArrayList<String> {
        LocationTypeUserTypes() {
            super(Arrays.stream(LocationType.ALL_USER_TYPES)
                  .map(LocationType::toString)
                  .collect(Collectors.toList()));
        }
    }
    static class LocationTypeConverter implements ITypeConverter<LocationType> {
        public LocationType convert(String value) throws Exception {
            return LocationType.createFromName(value);
        }
    }
    @Option(names = { "--locdefault" },
            description = "sets the default location type (allowed values: ${COMPLETION-CANDIDATES}) (default: ${DEFAULT-VALUE})",
            completionCandidates = LocationTypeUserTypes.class,
            converter = LocationTypeConverter.class,
            paramLabel = "loctype")
    // default value taken from declaration of
    // LocationTypeInferrerExtension.defaultType
    public LocationType defaultLocationType = LocationType.INFER;

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
        int exitCode = new CommandLine(new Absc()).execute(args);
        if (exitCode != 0) System.exit(exitCode);
    }

    @Override
    public Integer call() throws Exception {
        return new Main().mainMethod(this);
    }
}
