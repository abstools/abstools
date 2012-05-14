/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.parser;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.StringReader;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Map;
import java.util.jar.JarEntry;

import abs.frontend.mtvl.ChocoSolver;

import abs.common.Constants;
import abs.common.WrongProgramArgumentException;
import abs.frontend.analyser.SemanticError;
import abs.frontend.analyser.SemanticErrorList;
import abs.frontend.ast.CompilationUnit;
import abs.frontend.ast.List;
import abs.frontend.ast.Model;
import abs.frontend.ast.ModuleDecl;
import abs.frontend.ast.FeatureDecl;
import abs.frontend.ast.FExt;
import abs.frontend.ast.StarImport;
import abs.frontend.delta.exceptions.ASTNodeNotFoundException;
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
    protected boolean allowIncompleteExpr = false;
    protected LocationType defaultLocationType = null;
    protected boolean locationTypeInferenceEnabled = false;
    protected boolean fullabs = false;
    public String product;
    protected boolean locationTypeStats = false;
    protected LocationTypingPrecision locationTypeScope = null;
    // mTVL options
    protected boolean solve = false ;
    protected boolean solveall = false ;
    protected boolean check = false ;
    protected boolean numbersol = false ;
    protected boolean ignoreattr = false ;


    public static void main(final String... args)  {
       new Main().mainMethod(args);
    }
    
    public void mainMethod(final String... args) {
       try {
          parse(args);
       } catch (Exception e) {
          printErrorAndExit(e.getMessage());
       }
    }

    public void setWithStdLib(boolean withStdLib) {
        this.stdlib = withStdLib;
    }
    
    public void setAllowIncompleteExpr(boolean b) {
        allowIncompleteExpr = b;
    }
    
    public void setTypeChecking(boolean b) {
        typecheck = b;
    }
    
    public java.util.List<String> parseArgs(String[] args) throws Exception {
        ArrayList<String> remaindingArgs = new ArrayList<String>();

        for (String arg : args) {
            if (arg.equals("-dump"))
                dump = true;
            else if (arg.equals("-v"))
                verbose = true;
            else if (arg.equals("-version"))
                printVersionAndExit();
            else if (arg.startsWith("-product=")) {
                fullabs = true;
                product = arg.split("=")[1];
            }
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
            } else if (arg.equals("-solve")) {
                solve = true;
            } else if (arg.equals("-solveall")) {
                solveall = true;
            } else if (arg.equals("-sat")) {
                check = true;
            } else if (arg.startsWith("-check=")) {
                check = true;
                product = arg.split("=")[1];
            } else if (arg.equals("-nsol")) {
                numbersol = true;
            } else if (arg.equals("-noattr")) {
                ignoreattr = true;
            } else if (arg.equals("-h")) {
                printUsageAndExit();
            } else
                remaindingArgs.add(arg);
        }
        return remaindingArgs;
    }

    public Model parse(final String[] args) throws Exception {
        Model m = parseFiles(parseArgs(args).toArray(new String[0]));
        analyzeModel(m);
        return m;
    }

    public Model parseFiles(String... fileNames) throws IOException {
        if (fileNames.length == 0) {
            printErrorAndExit("Please provide at least one input file");
        }
    
        java.util.List<CompilationUnit> units = new ArrayList<CompilationUnit>();

        if (stdlib) {
            units.add(getStdLib());
        }

        for (String fileName : fileNames) {
            if (fileName.startsWith("-")) {
                throw new IllegalArgumentException("Illegal option " + fileName);
            }

            File f = new File(fileName);
            if (!f.canRead()) {
               throw new IllegalArgumentException("File "+fileName+" cannot be read");
            }
            
            if (!f.isDirectory() && !isABSSourceFile(f) && !isABSPackageFile(f)) {
               throw new IllegalArgumentException("File "+fileName+" is not a legal ABS file");
            }
        }
        
        for (String fileName : fileNames) {
           parseFileOrDirectory(units, new File(fileName));
        }

        List<CompilationUnit> unitList = new List<CompilationUnit>();
        for (CompilationUnit u : units) {
            unitList.add(u);
        }
        
        Model m = new Model(unitList);
        return m;
    }

    public void analyzeModel(Model m) throws WrongProgramArgumentException, ASTNodeNotFoundException {
        m.verbose = verbose;
        m.debug = dump;
        
        // drop attributes before calculating any attribute
        if (ignoreattr)
            m.dropAttributes();
        if (verbose) {
            System.out.println("Analyzing Model...");
        }
        // flatten before checking error, to avoid calculating *wrong* attributes
        if (fullabs) {
            if (typecheck)
                // apply deltas that correspond to given product
                m.flattenForProduct(product);
            else
                m.flattenForProductUnsafe(product);
        }
        if (dump) {
            m.dump();
            m.dumpMVars();
        }

        if (m.hasParserErrors()) {
            System.err.println("Syntactic errors: " + m.getParserErrors().size());
            for (ParserError e : m.getParserErrors()) {
                System.err.println(e.getHelpMessage());
                System.err.flush();
            }
        } else {
            final SemanticErrorList semErrs = m.getErrors();
            int numSemErrs = semErrs.size();

            if (numSemErrs > 0) {
                System.err.println("Semantic errors: " + numSemErrs);
                for (SemanticError error : semErrs) {
                    System.err.println(error.getHelpMessage());
                    System.err.flush();
                }
            } else {
//                if (fullabs) {
//                    // apply deltas that correspond to given product
//                    m.flattenForProduct(product);
//
//                    if (dump)
//                        m.dump();
//                }
                typeCheckModel(m);
                if (m.hasMTVL()) {
                    if (solve) {
                        if (verbose)
                            System.out.println("Searching for solutions for the feature model...");
                        ChocoSolver s = m.getCSModel();
                        System.out.print(s.resultToString());
                    }
                    if (solveall) {
                        if (verbose)
                            System.out.println("Searching for all solutions for the feature model...");
                        ChocoSolver s = m.getCSModel();
                        int i=1;
                        while(s.solveAgain()) {
                          System.out.println("------ "+(i++)+"------");
                          System.out.print(s.resultToString());
                        }
                    }
                    if (check) {
                        ChocoSolver s = m.getCSModel();
                        Map<String,Integer> guess = new HashMap<String,Integer>();
                        if (m.getSolution(product,guess))
                            System.out.println("checking solution: "+s.checkSolution(guess,m));
                        else {
                            System.out.println("Product '"+product+"' not found.");
                            if (!product.contains("."))
                                System.out.println("Maybe you forgot the module name?");
                        }
                    }
                    if (numbersol && !ignoreattr) {
                        ChocoSolver s = m.getCSModel();
                        System.out.println("Number of solutions found: "+s.countSolutions());
                      }
                    else if (numbersol && ignoreattr) {
                        ChocoSolver s = m.getCSModel();
                        System.out.println("Number of solutions found (without attributes): "+s.countSolutions());
                    }

                }
            }
        }
    }

    private void typeCheckModel(Model m) {
        if (typecheck) {
            if (verbose)
                System.out.println("Typechecking Model...");
            
            registerLocationTypeChecking(m);
            SemanticErrorList typeerrors = m.typeCheck();
            for (SemanticError se : typeerrors) {
                System.err.println(se.getHelpMessage());
            }
        }
    }

    private void registerLocationTypeChecking(Model m) {
        if (locationTypeInferenceEnabled) {
            if (verbose)
                System.out.println("Registering Location Type Checking...");
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
    }

    private void parseFileOrDirectory(java.util.List<CompilationUnit> units, File file) throws IOException {
        if (!file.canRead()) {
            System.err.println("WARNING: Could not read file "+file+", file skipped.");
        }
        
        if (file.isDirectory()) {
            parseDirectory(units, file);
        } else {
            if (isABSSourceFile(file))
                parseABSSourceFile(units,file);
            else if (isABSPackageFile(file))
                parseABSPackageFile(units,file);
        }
    }

    public java.util.List<CompilationUnit> parseABSPackageFile(File file) throws IOException {
        java.util.List<CompilationUnit> res = new ArrayList<CompilationUnit>();
        parseABSPackageFile(res, file);
        return res;
    }
    
    private void parseABSPackageFile(java.util.List<CompilationUnit> units, File file) throws IOException {
        ABSPackageFile jarFile = new ABSPackageFile(file);
        if (!jarFile.isABSPackage())
           return;
        Enumeration<JarEntry> e = jarFile.entries();
        while (e.hasMoreElements()) {
            JarEntry jarEntry = e.nextElement();
            if (!jarEntry.isDirectory()) {
                if (jarEntry.getName().endsWith(".abs")) {
                    parseABSSourceFile(units, "jar:"+file.toURI()+"!/"+jarEntry.getName(), jarFile.getInputStream(jarEntry));
                }
            }
        }
    }

    private void parseDirectory(java.util.List<CompilationUnit> units, File file) throws IOException {
        if (file.canRead() && !file.isHidden()) {
            for (File f : file.listFiles()) {
                if (f.isFile() && !isABSSourceFile(f) && !isABSPackageFile(f))
                    continue;
                parseFileOrDirectory(units, f);
            }
        }
    }

    public static boolean isABSPackageFile(File f) throws IOException {
       return f.getName().endsWith(".jar") && new ABSPackageFile(f).isABSPackage();
    }

    public static boolean isABSSourceFile(File f) {
        return f.getName().endsWith(".abs") || f.getName().endsWith(".mtvl");
    }

    private void parseABSSourceFile(java.util.List<CompilationUnit> units, String name, InputStream inputStream) throws IOException {
        parseABSSourceFile(units, new File(name), new InputStreamReader(inputStream, "UTF-8"));
    }

    private void parseABSSourceFile(java.util.List<CompilationUnit> units, File file) throws IOException {
        parseABSSourceFile(units, file, getUTF8FileReader(file));
    }
    
    private void parseABSSourceFile(java.util.List<CompilationUnit> units, File file, Reader reader) throws IOException {
        if (verbose)
            System.out.println("Parsing file "+file.getPath());//getAbsolutePath());
        units.add(parseUnit(file, null, reader));
    }

    protected void printErrorAndExit(String error) {
        System.err.println("\nCompilation failed:\n");
        System.err.println("  " + error);
        System.err.println();
        printUsageAndExit();
    }

    protected void printUsageAndExit() {
        printUsage();
        System.exit(1);
    }
    
    protected void printVersionAndExit() {
        System.out.println("ABS Tool Suite v"+getVersion());
        System.exit(1);
    }
    

    public CompilationUnit getStdLib() throws IOException {
        InputStream stream = Main.class.getClassLoader().getResourceAsStream(ABS_STD_LIB);
        if (stream == null) {
            printErrorAndExit("Could not find ABS Standard Library");
        }
        return parseUnit(new File(ABS_STD_LIB), null, new InputStreamReader(stream));
    }

    protected void printUsage() {
        printHeader();
        System.out.println(""
                + "Usage: java " + this.getClass().getName()
                + " [options] <absfiles>\n\n" 
                + "  <absfiles>     ABS files/directories/packages to parse\n\n" + "Options:\n"
                + "  -version       print version\n" 
                + "  -v             verbose output\n" 
                + "  -product=<PID> build given product by applying deltas (PID is the qualified product ID)\n"
                + "  -notypecheck   disable typechecking\n"
                + "  -nostdlib      do not include the standard lib\n"
                + "  -loctypes      enable location type checking\n"
                + "  -locdefault=<loctype> \n"
                + "                 sets the default location type to <loctype>\n"
                + "                 where <loctype> in " + Arrays.toString(LocationType.ALLUSERTYPES) + "\n"
                + "  -locscope=<scope> \n"
                + "                 sets the location aliasing scope to <scope>\n"
                + "                 where <scope> in " + Arrays.toString(LocationTypingPrecision.values()) + "\n"
                + "  -dump          dump AST to standard output \n" 
                + "  -solve         solve constraint satisfaction problem (CSP) for the feature model\n"
                + "  -solveall      get ALL solutions for the CSP\n"
                + "  -nsol          count the number of solutions\n"
                + "  -noattr        ignore the attributes\n"
                + "  -check=<PID>   check satisfiability of a product with qualified name PID\n" 
                + "  -h             print this message\n");
    }

    protected void printHeader() {
        
        String[] header = new String[] {
           "The ABS Compiler" + " v" + getVersion(),
           "Copyright (c) 2009-2011,    The HATS Consortium", 
           "All rights reserved. http://www.hats-project.eu" };
        
        int maxlength = header[1].length();
        StringBuilder starline = new StringBuilder();
        for (int i = 0; i < maxlength + 4; i++) {
            starline.append("*");
        }
        System.out.println(starline);
        for (String h : header) {
            System.out.print("* "+h);
            for (int i = 0; i < maxlength-h.length(); i++) {
                System.out.print(' ');
            }
            System.out.println(" *");
        }
        
        System.out.println(starline);
    }

    private String getVersion() {
        String version = Main.class.getPackage().getImplementationVersion();
        if (version == null)
            return "HEAD";
        else
            return version;
    }

    public CompilationUnit parseUnit(File file) throws Exception {
        Reader reader = getUTF8FileReader(file);
        BufferedReader rd = null;
        // Set to true to print source before parsing
        boolean dumpinput = false;
        if (dumpinput) {
            try {
                rd = getUTF8FileReader(file);
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

        return parseUnit(file, null, reader);
    }

    private BufferedReader getUTF8FileReader(File file) throws UnsupportedEncodingException, FileNotFoundException {
        return new BufferedReader(new InputStreamReader(new FileInputStream(file), "UTF-8"));
    }

    public static Iterable<File> toFiles(Iterable<String> fileNames) {
        ArrayList<File> files = new ArrayList<File>();
        for (String s : fileNames) {
            files.add(new File(s));
        }
        return files;
    }

    public Model parse(File file, String sourceCode, InputStream stream) throws Exception {
        return parse(file, sourceCode, new BufferedReader(new InputStreamReader(stream)));
    }

    public Model parse(File file, String sourceCode, Reader reader) throws Exception {
        List<CompilationUnit> units = new List<CompilationUnit>();
        if (stdlib)
            units.add(getStdLib());
        units.add(parseUnit(file, sourceCode, reader));
        return new Model(units);
    }

    public CompilationUnit parseUnit(File file, String sourceCode, Reader reader)
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
                u = new CompilationUnit(parser.getFileName(), new List<ModuleDecl>(), new List<FeatureDecl>(), new List<FExt>());
                u.setParserErrors(parser.getErrors());
            }
            if (stdlib) {
                for (ModuleDecl d : u.getModuleDecls()) {
                    if (!Constants.STDLIB_NAME.equals(d.getName()))
                        d.getImports().add(new StarImport(Constants.STDLIB_NAME));
                }
            }

            return u;
        } finally {
            reader.close();
        }
    }

    public static Model parseString(String s, boolean withStdLib) throws Exception {
        return parseString(s,withStdLib,false);
    }

    public static Model parseString(String s, boolean withStdLib, boolean allowIncompleteExpr) throws Exception {
        Main m = new Main();
        m.setWithStdLib(withStdLib);
        m.setAllowIncompleteExpr(allowIncompleteExpr);
        return m.parse(null, s, new StringReader(s));
    }

}
