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
import java.util.HashSet;
import java.util.Map;
import java.util.jar.JarEntry;

import javax.xml.parsers.ParserConfigurationException;

import choco.kernel.model.constraints.Constraint;
import abs.frontend.mtvl.ChocoSolver;
import abs.common.Constants;
import abs.common.WrongProgramArgumentException;
import abs.frontend.analyser.SemanticError;
import abs.frontend.analyser.SemanticErrorList;
import abs.frontend.antlr.parser.ABSParserWrapper;
import abs.frontend.ast.*;
import abs.frontend.configurator.preprocessor.ABSPreProcessor; //Preprocessor
import abs.frontend.configurator.visualizer.FMVisualizer;
import abs.frontend.delta.DeltaModellingException;
import abs.frontend.typechecker.locationtypes.LocationType;
import abs.frontend.typechecker.locationtypes.infer.LocationTypeInferrerExtension;
import abs.frontend.typechecker.locationtypes.infer.LocationTypeInferrerExtension.LocationTypingPrecision;

/**
 * @author rudi
 *
 */
public class Main {

    public static final String ABS_STD_LIB = "abs/lang/abslang.abs";
    protected boolean preprocess = false; //Preprocessor
    protected boolean verbose = false;
    protected boolean typecheck = true;
    protected boolean stdlib = true;
    protected boolean useJFlexAndBeaver = false;
    protected boolean dump = false;
    protected boolean debug = false;
    protected boolean allowIncompleteExpr = false;
    protected LocationType defaultLocationType = null;
    protected boolean locationTypeInferenceEnabled = false;
    // Must be public for AspectJ instrumentation
    public boolean fullabs = false;
    public String product;
    protected boolean locationTypeStats = false;
    protected LocationTypingPrecision locationTypeScope = null;
    // mTVL options
    protected boolean solve = false ;
    protected boolean solveall = false ;
    protected boolean solveWith = false ;
    protected boolean minWith = false ;
    protected boolean maxProduct = false ;
    protected boolean check = false ;
    protected boolean numbersol = false ;
    protected boolean ignoreattr = false ;
    protected boolean minimise = false ;
    protected boolean maximise = false ;


    public static void main(final String... args)  {
       new Main().mainMethod(args);
    }

    public void mainMethod(final String... args) {
       try {
           java.util.List<String> argslist = Arrays.asList(args);
           if (argslist.contains("-maude")) {
               abs.backend.maude.MaudeCompiler.main(args);
           } else if(argslist.contains("-java")) {
               abs.backend.java.JavaBackend.main(args);
           } else if (argslist.contains("-erlang")) {
               abs.backend.erlang.ErlangBackend.main(args);
           } else if (argslist.contains("-prolog")) {
               abs.backend.prolog.PrologBackend.main(args);
           } else if (argslist.contains("-coreabs")) {
               abs.backend.coreabs.CoreAbsBackend.main(args);
           } else if (argslist.contains("-prettyprint")) {
               abs.backend.prettyprint.PrettyPrinterBackEnd.main(args);
           } else if (argslist.contains("-keyabs")) {
               abs.backend.keyabs.KeyAbsBackend.main(args);
           } else if (argslist.contains("-outline")) {
               abs.backend.outline.OutlinePrinterBackEnd.main(args);
           } else if (argslist.contains("-dbschema")) {
               abs.backend.dbschema.GenerateDBSchema.main(args);
           } else {
               Model m = parse(args);
               if (m.hasParserErrors()) {
                   printParserErrorAndExit();
               }
           }
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

    public java.util.List<String> parseArgs(String[] args) {
        ArrayList<String> remainingArgs = new ArrayList<String>();

        for (String arg : args) {
            if (arg.equals("-dump"))
                dump = true;
            else if (arg.equals("-debug"))
                debug = true;
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
            else if (arg.equals("-with-old-parser"))
                useJFlexAndBeaver = true;
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
            } else if (arg.startsWith("-solveWith=")) {
                solveWith = true;
                product = arg.split("=")[1];
            } else if (arg.startsWith("-minWith=")) {
                minWith = true;
                product = arg.split("=")[1];
            } else if (arg.startsWith("-maxProduct")) {
                maxProduct = true;
            } else if (arg.startsWith("-min=")) {
                minimise = true;
                product = arg.split("=")[1];
            } else if (arg.startsWith("-max=")) {
                maximise = true;
                product = arg.split("=")[1];
            } else if (arg.equals("-sat")) {
                check = true;
            } else if (arg.startsWith("-check=")) {
                check = true;
                product = arg.split("=")[1];
            } else if (arg.equals("-nsol")) {
                numbersol = true;
            } else if (arg.equals("-noattr")) {
                ignoreattr = true;
            } else if (arg.equals("-preprocess")) { //Preprocessor
                    preprocess = true;
            } else if (arg.equals("-h") || arg.equals("-help")
                       || arg.equals("--help")) {
                printUsageAndExit();
            } else
                remainingArgs.add(arg);
        }
        return remainingArgs;
    }

    public Model parse(final String[] args) throws IOException, DeltaModellingException, WrongProgramArgumentException, ParserConfigurationException {
        Model m = parseFiles(parseArgs(args).toArray(new String[0]));
        analyzeModel(m);
        return m;
    }

    public Model parseFiles(String... fileNames) throws IOException {
        if (fileNames.length == 0) {
            printErrorAndExit("Please provide at least one input file");
        }

        java.util.List<CompilationUnit> units = new ArrayList<CompilationUnit>();

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

        if (stdlib)
            units.add(getStdLib());

        List<CompilationUnit> unitList = new List<CompilationUnit>();
        for (CompilationUnit u : units) {
            unitList.add(u);
        }

        Model m = new Model(unitList);
        Main.exceptionHack(m);
        return m;
    }

    public void analyzeModel(Model m) throws WrongProgramArgumentException, DeltaModellingException, FileNotFoundException, ParserConfigurationException {
        m.verbose = verbose;
        m.debug = dump;

        // drop attributes before calculating any attribute
        if (ignoreattr)
            m.dropAttributes();
        if (verbose) {
            System.out.println("Analyzing Model...");
        }
        //Preprocessor
        if (preprocess) {
            System.out.println("Preprocessing Model...");
            ABSPreProcessor oABSPreProcessor = new ABSPreProcessor();
            oABSPreProcessor.preProcessModel(m); //For Pre-processing...

            // Transformation of microTVL to Future Model Editor compatible XML
            FMVisualizer oFMVisualizer = new FMVisualizer();
            oFMVisualizer.ParseMicroTVLFile(m);
        }

        if (m.hasParserErrors()) {
            System.err.println("Syntactic errors: " + m.getParserErrors().size());
            for (ParserError e : m.getParserErrors()) {
                System.err.println(e.getHelpMessage());
                System.err.flush();
            }
        } else {
            rewriteModel(m, product);

            // type check PL before flattening
            // [ramus] disabled temporarily due to a bug
            //if (typecheck)
            //    typeCheckProductLine(m);

            // flatten before checking error, to avoid calculating *wrong* attributes
            if (fullabs) {
                if (typecheck)
                    // apply deltas that correspond to given product
                    m.flattenForProduct(product);
                else
                    m.flattenForProductUnsafe(product);
            }
            if (dump) {
                m.dumpMVars();
                m.dump();
            }

            final SemanticErrorList semErrs = m.getErrors();
            int numSemErrs = semErrs.size();

            if (numSemErrs > 0) {
                System.err.println("Semantic errors: " + numSemErrs);
                for (SemanticError error : semErrs) {
                    System.err.println(error.getHelpMessage());
                    System.err.flush();
                }
            } else {
                typeCheckModel(m);
                analyzeMTVL(m);
            }
        }
    }

    /**
     * Perform various rewrites that cannot be done in JastAdd
     *
     * JastAdd rewrite rules can only rewrite the current node using
     * node-local information.  ("The code in the body of the rewrite may
     * access and rearrange the nodes in the subtree rooted at A, but not any
     * other nodes in the AST. Furthermore, the code may not have any other
     * side effects." --
     * http://jastadd.org/web/documentation/reference-manual.php#Rewrites)
     *
     * We use this method to generate Exception constructors and the
     * information in ABS.Productline.
     *
     * @param m the model.
     * @param productname The name of the product.
     * @throws WrongProgramArgumentException
     */
    private static void rewriteModel(Model m, String productname)
        throws WrongProgramArgumentException
    {
        // Generate reflective constructors for all features
        ProductLine pl = m.getProductLine();
        if (pl != null) {
            // Let's assume the module and datatype names in abslang.abs did
            // not get changed, and just crash otherwise.  If you're here
            // because of a NPE: Hi!  Make the standard library and this code
            // agree about what the feature reflection module is called.
            ModuleDecl modProductline = null;
            DataTypeDecl featureDecl = null;
            FunctionDecl currentFeatureFun = null;
            FunctionDecl productNameFun = null;
            for (ModuleDecl d : m.getModuleDecls()) {
                if (d.getName().equals(Constants.PL_NAME)) {
                    modProductline = d;
                    break;
                }
            }
            for (Decl d : modProductline.getDecls()) {
                if (d instanceof DataTypeDecl && d.getName().equals("Feature")) {
                    featureDecl = (DataTypeDecl)d;
                } else if (d instanceof FunctionDecl && d.getName().equals("product_features")) {
                    currentFeatureFun = (FunctionDecl)d;
                } else if (d instanceof FunctionDecl && d.getName().equals("product_name")) {
                    productNameFun = (FunctionDecl)d;
                }
            }
            // Adjust Feature datatype
            for (Feature f : pl.getFeatures()) {
                // TODO: when/if we incorporate feature parameters into the
                // productline feature declarations (as we should), we need to
                // adjust the DataConstructor arguments here.
                featureDecl.addDataConstructor(new DataConstructor(f.getName(), new List<ConstructorArg>()));
            }
            // Adjust product_name() function
            productNameFun.setFunctionDef(new ExpFunctionDef(new StringLiteral(productname)));
            // Adjust product_features() function
            Product p = null;
            if (productname != null) p = m.findProduct(productname);
            if (p != null) {
                DataConstructorExp feature_arglist = new DataConstructorExp("Cons", new List<PureExp>());
                DataConstructorExp current = feature_arglist;
                for (Feature f : p.getFeatures()) {
                    DataConstructorExp next = new DataConstructorExp("Cons", new List<PureExp>());
                    // TODO: when/if we incorporate feature parameters into
                    // the productline feature declarations (as we should), we
                    // need to adjust the DataConstructorExp arguments here.
                    current.addParam(new DataConstructorExp(f.getName(), new List<PureExp>()));
                    current.addParam(next);
                    current = next;
                }
                current.setConstructor("Nil");
                currentFeatureFun.setFunctionDef(new ExpFunctionDef(feature_arglist));
            }
        }
    }

    /** Handle exceptions: add exceptions as constructors to the
     ABS.StdLib.Exception datatype.
     */
    public static void exceptionHack(Model m) {
        assert m != null;
        if (m.getExceptionType() == null) {
            return; // Eclipse?
        }
        DataTypeDecl e = (DataTypeDecl)(m.getExceptionType().getDecl());
        if (e != null) {
            // TODO: if null and not -nostdlib, throw an error
            for (Decl decl : m.getDecls()) {
                if (decl instanceof ExceptionDecl) {
                    ExceptionDecl e1 = (ExceptionDecl)decl;
                    // KLUDGE: what do we do about annotations to exceptions?
                    DataConstructor d = new DataConstructor(e1.getName(), e1.getConstructorArgs().fullCopy());
                    d.setPosition(e1.getStart(), e1.getEnd());
                    d.setFileName(e1.getFileName());
                    d.exceptionDecl = e1;
                    e1.dataConstructor = d;
                    e.addDataConstructor(d);
                }
            }
        }
    }


    /**
     * TODO: Should probably be introduced in Model through JastAdd by MTVL package.
     * However, the command-line argument handling will have to stay in Main. Pity.
     */
    private void analyzeMTVL(Model m) {
        Product p_product = null;
        try {
            p_product = product == null ? null : m.findProduct(product);
        } catch (WrongProgramArgumentException e) {
            // ignore in case we're just solving.
        }
        if (m.hasMTVL()) {
            if (solve) {
                if (verbose)
                    System.out.println("Searching for solutions for the feature model...");
                ChocoSolver s = m.instantiateCSModel();
                System.out.print(s.resultToString());
            }
            if (minimise) {
                assert product != null;
                if (verbose)
                    System.out.println("Searching for minimum solutions of "+product+" for the feature model...");
                ChocoSolver s = m.instantiateCSModel();
                System.out.print(s.minimiseToString(product));
            }
            if (maximise) {
                assert product != null;
                if (verbose)
                    System.out.println("Searching for maximum solutions of "+product+" for the feature model...");
                ChocoSolver s = m.instantiateCSModel();
                //System.out.print(s.maximiseToInt(product));
                s.addConstraint(ChocoSolver.eqeq(s.vars.get(product), s.maximiseToInt(product)));
                ChocoSolver s1 = m.instantiateCSModel();
                int i=1;
                while(s1.solveAgain()) {
                    System.out.println("------ "+(i++)+"------");
                    System.out.print(s1.resultToString());
                }
            }
            if (solveall) {
                if (verbose)
                    System.out.println("Searching for all solutions for the feature model...");
                ChocoSolver s = m.instantiateCSModel();
                int i=1;
                while(s.solveAgain()) {
                    System.out.println("------ "+(i++)+"------");
                    System.out.print(s.resultToString());
                }
            }
            if (solveWith) {
                assert product != null;
                if (verbose)
                    System.out.println("Searching for solution that includes "+product+"...");
                if (p_product != null) {
                    ChocoSolver s = m.instantiateCSModel();
                    HashSet<Constraint> newcs = new HashSet<Constraint>();
                    p_product.getProdConstraints(s.vars,newcs);
                    for (Constraint c: newcs) s.addConstraint(c);
                    System.out.println("checking solution: "+s.resultToString());
                } else {
                    System.out.println("Product '"+product+"' not found.");
                    if (!product.contains("."))
                        System.out.println("Maybe you forgot the module name?");
                }
            }
            if (minWith) {
                assert product != null;
                if (verbose)
                    System.out.println("Searching for solution that includes "+product+"...");
                ChocoSolver s = m.instantiateCSModel();
                HashSet<Constraint> newcs = new HashSet<Constraint>();
                s.addIntVar("difference", 0, 50);
                if (p_product != null) {
                    m.getDiffConstraints(p_product,s.vars,newcs, "difference");
                    for (Constraint c: newcs) s.addConstraint(c);
                    System.out.println("checking solution: "+s.minimiseToString("difference"));
                } else {
                    System.out.println("Product '"+product+"' not found.");
                    if (!product.contains("."))
                        System.out.println("Maybe you forgot the module name?");
                }

            }
            if (maxProduct) {
                assert product != null;
                if (verbose)
                    System.out.println("Searching for solution that includes "+product+"...");
                ChocoSolver s = m.instantiateCSModel();
                HashSet<Constraint> newcs = new HashSet<Constraint>();
                s.addIntVar("noOfFeatures", 0, 50);
                if (m.getMaxConstraints(s.vars,newcs, "noOfFeatures")) {
                    for (Constraint c: newcs) s.addConstraint(c);
                    System.out.println("checking solution: "+s.maximiseToString("noOfFeatures"));
                }
                else {
                    System.out.println("---No solution-------------");
                }

            }
            if (check) {
                assert product != null;
                ChocoSolver s = m.instantiateCSModel();
                if (p_product == null ){
                    System.out.println("Product '"+product+"' not found.");
                    if (!product.contains("."))
                        System.out.println("Maybe you forgot the module name?");
                } else {
                    Map<String,Integer> guess = p_product.getSolution();
                    System.out.println("checking solution: "+s.checkSolution(guess,m));
                }
            }
            if (numbersol && !ignoreattr) {
                ChocoSolver s = m.instantiateCSModel();
                System.out.println("Number of solutions found: "+s.countSolutions());
            }
            else if (numbersol && ignoreattr) {
                ChocoSolver s = m.instantiateCSModel();
                System.out.println("Number of solutions found (without attributes): "+s.countSolutions());
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

    private void typeCheckProductLine(Model m) {

        if (verbose)
            System.out.println("Typechecking Software Product Line...");

        SemanticErrorList typeerrors = m.typeCheckPL();
        for (SemanticError se : typeerrors) {
            System.err.println(se.getHelpMessage());
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
        try {
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
        } finally {
            jarFile.close();
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
       final ABSPackageFile absPackageFile = new ABSPackageFile(f);
       final boolean isPackage = absPackageFile.isABSPackage();
       absPackageFile.close();
       return f.getName().endsWith(".jar") && isPackage;
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

    protected static void printParserErrorAndExit() {
        System.err.println("\nCompilation failed with syntax errors.");
        System.exit(1);
    }

    protected static void printErrorAndExit(String error) {
        System.err.println("\nCompilation failed:\n");
        System.err.println("  " + error);
        System.err.println();
        System.exit(1);
    }

    protected void printUsageAndExit() {
        printUsage();
        System.exit(1);
    }

    protected static void printVersionAndExit() {
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
                + " [backend] [options] <absfiles>\n\n"
                + "  <absfiles>     ABS files/directories/packages to parse\n\n"
                + "Available backends:\n"
                + "  -maude         generate Maude code\n"
                + "  -java          generate Java code\n"
                + "  -erlang        generate Erlang code\n"
                + "  -haskell       generate Haskell code\n" // this is just for help printing; the execution of the compiler is done by the bash scipt absc
                + "  -prolog        generate Prolog\n"
                + "  -prettyprint   pretty-print ABS code\n\n"
                + "type 'absc -<backend> -help' to see backend-specific options\n\n"
                + "Common options:\n"
                + "  -version       print version\n"
                + "  -v             verbose output\n"
                + "  -product=<PID> build given product by applying deltas\n"
                + "                 (PID is the qualified product ID)\n"
                + "  -notypecheck   disable typechecking\n"
                + "  -nostdlib      do not include the standard lib\n"
                + "  --with-old-parser\n"
                + "                 use old (deprecated) parser implementation\n"
                + "  -loctypes      enable location type checking\n"
                + "  -locdefault=<loctype> \n"
                + "                 sets the default location type to <loctype>\n"
                + "                 where <loctype> in " + Arrays.toString(LocationType.ALLUSERTYPES) + "\n"
                + "  -locscope=<scope> \n"
                + "                 sets the location aliasing scope to <scope>\n"
                + "                 where <scope> in " + Arrays.toString(LocationTypingPrecision.values()) + "\n"
                + "  -dump          dump AST to standard output \n"
                + "  -solve         solve constraint satisfaction problem (CSP) for the feature\n"
                + "                 model\n"
                + "  -solveall      get ALL solutions for the CSP\n"
                + "  -solveWith=<PID>\n"
                + "                 solve CSP by finding a product that includes PID.\n"
                + "  -min=<var>     minimise variable <var> when solving the CSP for the feature\n"
                + "                 model\n"
                + "  -max=<var>     maximise variable <var> when solving the CSP for the feature\n"
                + "                 model\n"
                + "  -maxProduct    get the solution that has the most number of features\n"
                + "  -minWith=<PID> \n"
                + "                 solve CSP by finding a solution that tries to include PID\n"
                + "                 with minimum number of changes.\n"
                + "  -nsol          count the number of solutions\n"
                + "  -noattr        ignore the attributes\n"
                + "  -check=<PID>   check satisfiability of a product with qualified name PID\n"
                + "  -preprocess    Preprocessing the Model\n" //Preprocessor
                + "  -h             print this message\n");
    }

    protected static void printHeader() {

        String[] header = new String[] {
           "The ABS Compiler" + " v" + getVersion(),
           "Copyright (c) 2009-2013,    The HATS Consortium",
           "Copyright (c) 2013-2015,    The Envisage Project",
           "http://www.abs-models.org/" };

        int maxlength = header[0].length();
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

    private static String getVersion() {
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

    public Model parse(File file, String sourceCode, InputStream stream) throws IOException {
        return parse(file, sourceCode, new BufferedReader(new InputStreamReader(stream)));
    }

    public Model parse(File file, String sourceCode, Reader reader) throws IOException  {
        List<CompilationUnit> units = new List<CompilationUnit>();
        if (stdlib)
            units.add(getStdLib());
        units.add(parseUnit(file, sourceCode, reader));
        return new Model(units);
    }

    public CompilationUnit parseUnit(File file, String sourceCode, Reader reader)
            throws IOException
    {
        try {
            return new ABSParserWrapper(file, false, stdlib, useJFlexAndBeaver)
                .parse(reader);
        } finally {
            reader.close();
        }
    }

    public static Model parseString(String s, boolean withStdLib) throws Exception {
        return parseString(s,withStdLib,false);
    }

    /**
     * Calls {@link #parseString(String, boolean, boolean, boolean)} with withDbLib set to false.
     *
     * @param s
     * @param withStdLib
     * @param allowIncompleteExpr
     * @return Model
     * @throws Exception
     */
    public static Model parseString(String s, boolean withStdLib, boolean allowIncompleteExpr) throws Exception {
        Main m = new Main();
        m.setWithStdLib(withStdLib);
        m.setAllowIncompleteExpr(allowIncompleteExpr);
        return m.parse(null, s, new StringReader(s));
    }

}
