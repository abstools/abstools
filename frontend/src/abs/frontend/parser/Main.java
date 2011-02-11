//$Id$

package abs.frontend.parser;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.Arrays;

import abs.common.Constants;
import abs.frontend.analyser.SemanticError;
import abs.frontend.analyser.SemanticErrorList;
import abs.frontend.ast.CompilationUnit;
import abs.frontend.ast.List;
import abs.frontend.ast.Model;
import abs.frontend.ast.ModuleDecl;
import abs.frontend.ast.StarImport;
import abs.frontend.delta.*;
import abs.frontend.typechecker.locationtypes.LocationType;
import abs.frontend.typechecker.locationtypes.infer.LocationTypeInferrerExtension;
import abs.frontend.typechecker.locationtypes.infer.LocationTypeInferrerExtension.LocationTypingPrecision;
import beaver.Parser;

public class Main {

    public static final String ABS_STD_LIB = "abs/lang/abslang.abs";
    protected boolean verbose = false;
    protected boolean typecheck = true;
    protected boolean stdlib = true;
    protected boolean dump = false;
    protected LocationType defaultLocationType = null;
    protected boolean locationTypeInferenceEnabled = false;
    protected boolean fullabs = false;
    protected boolean locationTypeStats = false;
    protected LocationTypingPrecision locationTypeScope = null;

    public static void main(final String... args) throws Exception {
        new Main().parse(args);
    }

    public java.util.List<String> parseArgs(String[] args) throws Exception {
        ArrayList<String> remaindingArgs = new ArrayList<String>();

        for (String arg : args) {
            if (arg.equals("-dump"))
                dump = true;
            else if (arg.equals("-v"))
                verbose = true;
            else if (arg.equals("-fullabs"))
                fullabs = true;
            else if (arg.equals("-notypecheck"))
                typecheck = false;
            else if (arg.equals("-nostdlib"))
                stdlib = false;
            else if (arg.equals("-loctypestats"))
                locationTypeStats = true;
            else if (arg.equals("-loctypes")) {
                locationTypeInferenceEnabled = true;
            } else if (arg.startsWith("-locdefault=")) {
                String def = arg.split("=")[1];
                defaultLocationType = LocationType.createFromName(def);
            } else if (arg.startsWith("-locscope=")) {
                String def = arg.split("=")[1];
                locationTypeScope = LocationTypingPrecision.valueOf(def);
            } else if (arg.equals("-h")) {
                printUsageAndExit();
            } else
                remaindingArgs.add(arg);

        }
        return remaindingArgs;
    }

    public Model parse(final String[] args) throws Exception {

        java.util.List<String> files = parseArgs(args);

        if (files.isEmpty()) {
            printErrorAndExit("Please provide at least one intput file");
        }

        List<CompilationUnit> units = new List<CompilationUnit>();

        if (stdlib) {
            units.add(getStdLib());
        }

        for (String file : files) {
            if (file.startsWith("-")) {
                printErrorAndExit("Illegal option " + file);
            }

            try {
                units.add(parseUnit(new File(file), stdlib));

            } catch (FileNotFoundException e1) {
                printErrorAndExit("File not found: " + file);
            } catch (ParseException pex) {
                System.err.println(pex.getError().getHelpMessage());
                System.exit(1);
            } catch (Exception e1) {
                // Catch-all
                System.err.println("Compilation of " + file + " failed with Exception");
                System.err.println(e1);
                System.exit(1);
            }
        }

        Model m = new Model(units);

        if (dump) {
            m.dump();
        }

        if (m.hasParserErrors()) {
            System.err.println("Syntactic errors: " + m.getParserErrors().size());
            for (ParserError e : m.getParserErrors()) {
                System.err.println(e.getHelpMessage());
                System.err.flush();
            }
        } else {
            int numSemErrs = m.getErrors().size();

            if (numSemErrs > 0) {
                System.err.println("Semantic errors: " + numSemErrs);
                for (SemanticError error : m.getErrors()) {
                    System.err.println(error.getHelpMessage());
                    System.err.flush();
                }
            } else {
                if (fullabs) {
                    ArrayList<Delta> deltas = new ArrayList<Delta>();
                    m.readDeltas(deltas);
                    
                    for (Delta delta : deltas)
                        System.out.println(delta);
                    
                    for (Delta delta : deltas)
                        m.applyDelta(delta);
                    
                    if (dump)
                        m.dump();
                }
                if (typecheck) {
                    if (locationTypeInferenceEnabled) {
                        LocationTypeInferrerExtension ltie = new LocationTypeInferrerExtension(m);
                        if (locationTypeStats) {
                            ltie.enableStatistics();
                        }
                        if (defaultLocationType != null) {
                            ltie.setDefaultType(defaultLocationType);
                        }
                        if (locationTypeScope != null) {
                            ltie.setLocationTypingPrecision(locationTypeScope);
                        }
                        m.registerTypeSystemExtension(ltie);
                    }
                    SemanticErrorList typeerrors = m.typeCheck();
                    for (SemanticError se : typeerrors) {
                        System.err.println(se.getHelpMessage());
                    }
                }
                if (verbose) {
                    System.out.println(m);
                }
            }
        }

        return m;
    }

    private void printErrorAndExit(String error) {
        System.err.println("\nCompilation failed:\n");
        System.err.println("  " + error);
        System.err.println();
        printUsageAndExit();
    }

    protected void printUsageAndExit() {
        printUsage();
        System.exit(1);
    }

    public static CompilationUnit getStdLib() throws IOException {
        InputStream stream = Main.class.getClassLoader().getResourceAsStream(ABS_STD_LIB);
        if (stream == null) {
            System.err.println("Could not found ABS Standard Library");
            System.exit(1);
        }
        return parseUnit(new File(ABS_STD_LIB), null, new InputStreamReader(stream), false);
    }

    protected void printUsage() {
        System.out.println(
                  "*******************************\n" 
                + "*        ABS TOOL SUITE       *\n"
                + "*******************************\n" 
                + "Usage: java " + this.getClass().getName()
                + " [options] <absfiles>\n\n" 
                + "  <absfiles>     ABS files to parse\n\n" + "Options:\n"
                + "  -v             verbose output\n" 
                + "  -notypecheck   disable typechecking\n"
                + "  -nostdlib      do not include the standard lib \n"
                + "  -loctypes      enable location type checking\n"
                + "  -locdefault=<loctype> \n"
                + "                 sets the default location type to <loctype>\n"
                + "                 where <loctype> in " + Arrays.toString(LocationType.ALLUSERTYPES) + "\n"
                + "  -locscope=<scope> \n"
                + "                 sets the location aliasing scope to <scope>\n"
                + "                 where <scope> in " + Arrays.toString(LocationTypingPrecision.values()) + "\n"
                + "  -dump          dump AST to standard output \n" 
                + "  -h             print this message\n");
    }

    public static CompilationUnit parseUnit(File file, boolean withStdLib) throws Exception {
        Reader reader = new FileReader(file);
        BufferedReader rd = null;
        // Set to true to print source before parsing
        boolean dumpinput = false;
        if (dumpinput) {
            try {
                rd = new BufferedReader(new FileReader(file));
                String line = null;
                int i = 1;
                while ((line = rd.readLine()) != null) {
                    System.out.println(i++ + "\t" + line);
                }
            } catch (IOException x) {
                System.out.flush();
                System.err.println(x);
                System.err.flush();
            } finally {
                if (rd != null)
                    rd.close();
            }
        }

        return parseUnit(file, null, reader, withStdLib);
    }

    public static Model parse(File file, boolean withStdLib) throws Exception {
        List<CompilationUnit> units = new List<CompilationUnit>();
        if (withStdLib)
            units.add(getStdLib());
        units.add(parseUnit(file, withStdLib));
        return new Model(units);
    }

    public static Model parse(ArrayList<String> fileNames, boolean withStdLib) throws Exception {
        List<CompilationUnit> units = new List<CompilationUnit>();
        if (withStdLib)
            units.add(getStdLib());
        for (String filename : fileNames) {
            units.add(parseUnit(new File(filename), withStdLib));
        }
        return new Model(units);
    }

    public static Model parse(File file, String sourceCode, InputStream stream, boolean withStdLib) throws Exception {
        return parse(file, sourceCode, new BufferedReader(new InputStreamReader(stream)), withStdLib);
    }

    public static Model parse(File file, String sourceCode, Reader reader, boolean withStdLib) throws Exception {
        return parse(file, sourceCode, reader, withStdLib, false);
    }
    
    public static Model parse(File file, String sourceCode, Reader reader, boolean withStdLib, boolean allowIncompleteExpr) throws Exception {
        List<CompilationUnit> units = new List<CompilationUnit>();
        if (withStdLib)
            units.add(getStdLib());
        units.add(parseUnit(file, sourceCode, reader, withStdLib, allowIncompleteExpr));
        return new Model(units);
    }

    public static CompilationUnit parseUnit(File file, String sourceCode, Reader reader, boolean importStdLib)
    throws IOException {
         return parseUnit(file, sourceCode, reader, importStdLib, false);
    }
    
    public static CompilationUnit parseUnit(File file, String sourceCode, Reader reader, boolean importStdLib, boolean allowIncompleteExpr)
            throws IOException {
        try {
            ABSParser parser = new ABSParser();
            ABSScanner scanner = new ABSScanner(reader);
            parser.setSourceCode(sourceCode);
            parser.setFile(file);
            parser.allowIncompleteExpr(allowIncompleteExpr);

            CompilationUnit u = null;
            try {
                u = (CompilationUnit) parser.parse(scanner);
            } catch (Parser.Exception e) {
                u = new CompilationUnit(parser.getFileName(), new List());
                u.setParserErrors(parser.getErrors());
            }
            if (importStdLib) {
                for (ModuleDecl d : u.getModuleDecls()) {
                    d.getImports().add(new StarImport(Constants.STDLIB_NAME));
                }
            }

            return u;
        } finally {
            reader.close();
        }
    }

    public static Model parseString(String s, boolean withStdLib) throws Exception {
        return parse(null, s, new StringReader(s), withStdLib);
    }

    public static Model parseString(String s, boolean withStdLib, boolean allowIncompleteExpr) throws Exception {
        return parse(null, s, new StringReader(s), withStdLib, allowIncompleteExpr);
    }
    
}
