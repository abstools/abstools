/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.frontend.parser;

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

import org.abs_models.backend.common.InternalBackendException;
import org.abs_models.backend.coreabs.CoreAbsBackend;
import org.abs_models.backend.erlang.ErlangBackend;
import org.abs_models.backend.java.JavaBackend;
import org.abs_models.backend.maude.MaudeCompiler;
import org.abs_models.backend.outline.OutlinePrinterBackEnd;
import org.abs_models.backend.prettyprint.PrettyPrinterBackEnd;
import org.abs_models.backend.prolog.PrologBackend;
import org.abs_models.common.Constants;
import org.abs_models.common.WrongProgramArgumentException;
import org.abs_models.frontend.analyser.SemanticCondition;
import org.abs_models.frontend.analyser.SemanticConditionList;
import org.abs_models.frontend.antlr.parser.ABSLexer;
import org.abs_models.frontend.antlr.parser.ABSParser;
import org.abs_models.frontend.antlr.parser.CreateJastAddASTListener;
import org.abs_models.frontend.antlr.parser.SyntaxErrorCollector;
import org.abs_models.frontend.ast.CompilationUnit;
import org.abs_models.frontend.ast.DataConstructor;
import org.abs_models.frontend.ast.DataConstructorExp;
import org.abs_models.frontend.ast.DataTypeDecl;
import org.abs_models.frontend.ast.Decl;
import org.abs_models.frontend.ast.ExpFunctionDef;
import org.abs_models.frontend.ast.Feature;
import org.abs_models.frontend.ast.FromImport;
import org.abs_models.frontend.ast.FunctionDecl;
import org.abs_models.frontend.ast.Import;
import org.abs_models.frontend.ast.List;
import org.abs_models.frontend.ast.Model;
import org.abs_models.frontend.ast.ModuleDecl;
import org.abs_models.frontend.ast.Opt;
import org.abs_models.frontend.ast.ProductDecl;
import org.abs_models.frontend.ast.ProductLine;
import org.abs_models.frontend.ast.StarImport;
import org.abs_models.frontend.ast.StringLiteral;
import org.abs_models.frontend.delta.DeltaModellingException;
import org.abs_models.frontend.delta.ProductLineAnalysisHelper;
import org.abs_models.frontend.mtvl.ChocoSolver;
import org.abs_models.frontend.typechecker.locationtypes.LocationType;
import org.abs_models.frontend.typechecker.locationtypes.infer.LocationTypeInferrerExtension;
import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeWalker;

import choco.kernel.model.constraints.Constraint;

/**
 * @author rudi
 *
 */
public class Main {

    public static final String ABS_STD_LIB = "abs/lang/abslang.abs";
    public static final String UNKNOWN_FILENAME = "<unknown file>";
    protected boolean verbose = false;
    protected boolean typecheck = true;
    protected boolean checkspl = false;
    protected boolean dump = false;
    protected boolean debug = false;
    protected LocationType defaultLocationType = null;
    protected boolean locationTypeInferenceEnabled = false;
    // Must be public for AspectJ instrumentation
    public boolean fullabs = false;
    public String product;
    protected boolean locationTypeStats = false;
    protected LocationTypeInferrerExtension.LocationTypingPrecision locationTypeScope = null;
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
        int result = new Main().mainMethod(args);
        if (result != 0) System.exit(result);
    }

    public int mainMethod(final String... args) {
        int result = 0;
        try {
            java.util.List<String> argslist = Arrays.asList(args);
            if (argslist.contains("-help") || argslist.contains("-h") || argslist.contains("--help")) {
                printUsage();
            } else if (argslist.contains("-version") || argslist.contains("--version")) {
                printVersion();
            } else if (argslist.contains("-maude")) {
                result = MaudeCompiler.doMain(args);
            } else if(argslist.contains("-java")) {
                result = JavaBackend.doMain(args);
            } else if (argslist.contains("-erlang")) {
                result = ErlangBackend.doMain(args);
            } else if (argslist.contains("-prolog")) {
                result = PrologBackend.doMain(args);
            } else if (argslist.contains("-coreabs")) {
                result = CoreAbsBackend.doMain(args);
            } else if (argslist.contains("-prettyprint")) {
                result = PrettyPrinterBackEnd.doMain(args);
            } else if (argslist.contains("-outline")) {
                result = OutlinePrinterBackEnd.doMain(args);
            } else {
                Model m = parse(args);
                if (m.hasParserErrors() || m.hasErrors() || m.hasTypeErrors()) {
                    printErrorMessage();
                    result = 1;
                }
            }
        } catch (InternalBackendException e) {
            // don't print stack trace here
            printError(e.getMessage());
            result = 1;
        } catch (Exception e) {
            if (e.getMessage() == null) { e.printStackTrace(); }
            assert e.getMessage() != null : e.toString();
            printError(e.getMessage());
            result = 1;
        }
        return result;
    }

    public java.util.List<String> parseArgs(String[] args) throws InternalBackendException {
        ArrayList<String> remainingArgs = new ArrayList<>();

        for (String arg : args) {
            if (arg.equals("-dump"))
                dump = true;
            else if (arg.equals("-debug"))
                debug = true;
            else if (arg.equals("-v"))
                verbose = true;
            else if (arg.startsWith("-product=")) {
                fullabs = true;
                product = arg.split("=")[1];
            } else if (arg.startsWith("-allproducts")) {
                fullabs = true;
                product = null;
            } else if (arg.equals("-checkspl"))
                checkspl = true;
            else if (arg.equals("-notypecheck"))
                typecheck = false;
            else if (arg.equals("-loctypestats"))
                locationTypeStats = true;
            else if (arg.equals("-loctypes")) {
                locationTypeInferenceEnabled = true;
            } else if (arg.startsWith("-locdefault=")) {
                String def = arg.split("=")[1];
                defaultLocationType = LocationType.createFromName(def);
            } else if (arg.startsWith("-locscope=")) {
                String def = arg.split("=")[1];
                locationTypeScope = LocationTypeInferrerExtension.LocationTypingPrecision.valueOf(def);
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
            } else if (arg.equals("-h") || arg.equals("-help")
                    || arg.equals("--help")) {
                printUsage();
            } else
                remainingArgs.add(arg);
        }
        return remainingArgs;
    }

    public Model parse(final String[] args) throws IOException, DeltaModellingException, WrongProgramArgumentException, InternalBackendException {
        Model m = parseFiles(verbose, parseArgs(args).toArray(new String[0]));
        analyzeFlattenAndRewriteModel(m);
        return m;
    }

    public static Model parseFiles(boolean verbose, String... fileNames) throws IOException, InternalBackendException {
        if (fileNames.length == 0) {
            throw new IllegalArgumentException("Please provide at least one input file");
        }

        java.util.List<CompilationUnit> units = new ArrayList<>();

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
            parseFileOrDirectory(units, new File(fileName), verbose);
        }

	units.add(getStdLib());

        List<CompilationUnit> unitList = new List<>();
        for (CompilationUnit u : units) {
            unitList.add(u);
        }

        Model m = new Model(unitList);
        return m;
    }

    /**
     * This horrible method does too many things and needs to be in every code
     * path that expects a working model, especially when products are
     * involved.  (ProductDecl.getProduct() returns null until
     * evaluateAllProductDeclarations() was called once.)
     *
     * @param m
     * @throws WrongProgramArgumentException
     * @throws DeltaModellingException
     * @throws FileNotFoundException
     */
    public void analyzeFlattenAndRewriteModel(Model m) throws WrongProgramArgumentException, DeltaModellingException, FileNotFoundException {
        m.verbose = verbose;
        m.debug = debug;

        // drop attributes before calculating any attribute
        if (ignoreattr)
            m.dropAttributes();

        if (verbose) {
            System.out.println("Analyzing Model...");
        }

        if (m.hasParserErrors()) {
            System.err.println("Syntactic errors: " + m.getParserErrors().size());
            for (ParserError e : m.getParserErrors()) {
                System.err.println(e.getHelpMessage());
                System.err.flush();
            }
            return;
        }

        m.evaluateAllProductDeclarations(); // resolve ProductExpressions to simple sets of features
        rewriteModel(m, product);
        m.flattenTraitOnly();
        m.collapseTraitModifiers();

        m.expandPartialFunctions();

        // check PL before flattening
        if (checkspl)
            typeCheckProductLine(m);

        // flatten before checking error, to avoid calculating *wrong* attributes
        if (fullabs) {
            if (product == null) {
                // Build all SPL configurations (valid feature selections, ignoring attributes), one by one (for performance measuring)
                if (verbose)
                    System.out.println("Building ALL " + m.getProductList().getNumChild() + " feature model configurations...");
                ProductLineAnalysisHelper.buildAllConfigurations(m);
                return;
            }
            if (typecheck)
                // apply deltas that correspond to given product
                m.flattenForProduct(product);
            else
                m.flattenForProductUnsafe(product);
        }

        if (dump) {
            m.dumpMVars();
            m.dump(System.out);
        }

        final SemanticConditionList semErrs = m.getErrors();

        if (semErrs.containsErrors()) {
            System.err.println("Semantic errors: " + semErrs.getErrorCount());
        }
        for (SemanticCondition error : semErrs) {
            // Print both errors and warnings
            System.err.println(error.getHelpMessage());
            System.err.flush();
        }
        if (!semErrs.containsErrors()) {
            typeCheckModel(m);
            analyzeMTVL(m);
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
                featureDecl.addDataConstructor(new DataConstructor(f.getName(), new List<>()));
            }
            // Adjust product_name() function
            productNameFun.setFunctionDef(new ExpFunctionDef(new StringLiteral(productname)));
            // Adjust product_features() function
            ProductDecl p = null;
            if (productname != null) p = m.findProduct(productname);
            if (p != null) {
                DataConstructorExp feature_arglist = new DataConstructorExp("Cons", new List<>());
                DataConstructorExp current = feature_arglist;
                for (Feature f : p.getProduct().getFeatures()) {
                    DataConstructorExp next = new DataConstructorExp("Cons", new List<>());
                    // TODO: when/if we incorporate feature parameters into
                    // the productline feature declarations (as we should), we
                    // need to adjust the DataConstructorExp arguments here.
                    current.addParamNoTransform(new DataConstructorExp(f.getName(), new List<>()));
                    current.addParamNoTransform(next);
                    current = next;
                }
                current.setConstructor("Nil");
                currentFeatureFun.setFunctionDef(new ExpFunctionDef(feature_arglist));
            }
        }
        m.flushTreeCache();
    }

    /**
     * TODO: Should probably be introduced in Model through JastAdd by MTVL package.
     * However, the command-line argument handling will have to stay in Main. Pity.
     */
    private void analyzeMTVL(Model m) {
        ProductDecl productDecl = null;
        try {
            productDecl = product == null ? null : m.findProduct(product);
        } catch (WrongProgramArgumentException e) {
            // ignore in case we're just solving.
        }
        if (m.hasMTVL()) {
            if (solve) {
                if (verbose)
                    System.out.println("Searching for solutions for the feature model...");
                ChocoSolver s = m.instantiateCSModel();
                System.out.print(s.getSolutionsAsString());
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
                    System.out.print(s1.getSolutionsAsString());
                }
            }
            if (solveall) {
                if (verbose)
                    System.out.println("Searching for all solutions for the feature model...");
                ChocoSolver solver = m.instantiateCSModel();
                System.out.print(solver.getSolutionsAsString());
            }
            if (solveWith) {
                assert product != null;
                if (verbose)
                    System.out.println("Searching for solution that includes " + product + "...");
                if (productDecl != null) {
                    ChocoSolver s = m.instantiateCSModel();
                    HashSet<Constraint> newcs = new HashSet<>();
                    productDecl.getProduct().getProdConstraints(s.vars, newcs);
                    for (Constraint c: newcs)
                        s.addConstraint(c);
                    System.out.println("checking solution:\n" + s.getSolutionsAsString());
                } else {
                    System.out.println("Product '" + product + "' not found.");
                }
            }
            if (minWith) {
                assert product != null;
                if (verbose)
                    System.out.println("Searching for solution that includes " + product + "...");
                ChocoSolver s = m.instantiateCSModel();
                HashSet<Constraint> newcs = new HashSet<>();
                s.addIntVar("difference", 0, 50);
                if (productDecl != null) {
                    m.getDiffConstraints(productDecl.getProduct(), s.vars, newcs, "difference");
                    for (Constraint c: newcs) s.addConstraint(c);
                    System.out.println("checking solution: " + s.minimiseToString("difference"));
                } else {
                    System.out.println("Product '" + product + "' not found.");
                }

            }
            if (maxProduct) {
                assert product != null;
                if (verbose)
                    System.out.println("Searching for solution that includes "+product+"...");
                ChocoSolver s = m.instantiateCSModel();
                HashSet<Constraint> newcs = new HashSet<>();
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
                if (productDecl == null ){
                    System.out.println("Product '" + product + "' not found.");
                } else {
                    Map<String,Integer> guess = productDecl.getProduct().getSolution();
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
            SemanticConditionList typeerrors = m.typeCheck();
            for (SemanticCondition se : typeerrors) {
                System.err.println(se.getHelpMessage());
            }
        }
    }

    private void registerLocationTypeChecking(Model m) {
        if (locationTypeInferenceEnabled) {
            if (verbose)
                System.out.println("Registering Location Type Checking...");
            LocationTypeInferrerExtension ltie = new LocationTypeInferrerExtension(m);
            if (locationTypeStats || verbose) {
                ltie.enableStatistics();
            }
            if (debug) {
                ltie.enableDebugOutput();
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

        //int n = m.getFeatureModelConfigurations().size();
        int n = m.getProductList().getNumChild();
        if (n == 0)
            return;

        if (verbose) {
            System.out.println("Typechecking Software Product Line (" + n + " products)...");
        }
        SemanticConditionList errors = m.typeCheckPL();
        for (SemanticCondition err : errors) {
            System.err.println(err.getHelpMessage());
        }
    }

    private static void parseFileOrDirectory(java.util.List<CompilationUnit> units, File file, boolean verbose)
	throws IOException
    {
	if (!file.canRead()) {
	    System.err.println("WARNING: Could not read file "+file+", file skipped.");
	}

        if (file.isDirectory()) {
            parseDirectory(units, file, verbose);
        } else {
            if (isABSSourceFile(file))
                parseABSSourceFile(units,file, verbose);
            else if (isABSPackageFile(file))
                parseABSPackageFile(units,file, verbose);
        }
    }

    private static void parseABSPackageFile(java.util.List<CompilationUnit> units, File file, boolean verbose) throws IOException {
        ABSPackageFile jarFile = new ABSPackageFile(file);
        try {
            if (!jarFile.isABSPackage())
                return;
            Enumeration<JarEntry> e = jarFile.entries();
            while (e.hasMoreElements()) {
                JarEntry jarEntry = e.nextElement();
                if (!jarEntry.isDirectory()) {
                    if (jarEntry.getName().endsWith(".abs")) {
                        parseABSSourceFile(units, "jar:"+file.toURI()+"!/"+jarEntry.getName(), jarFile.getInputStream(jarEntry), verbose);
                    }
                }
            }
        } finally {
            jarFile.close();
        }
    }

    private static void parseDirectory(java.util.List<CompilationUnit> units, File file, boolean verbose) throws IOException {
        if (file.canRead() && !file.isHidden()) {
            for (File f : file.listFiles()) {
                if (f.isFile() && !isABSSourceFile(f) && !isABSPackageFile(f))
                    continue;
                parseFileOrDirectory(units, f, verbose);
            }
        }
    }

    public static boolean isABSPackageFile(File f) {
        ABSPackageFile absPackageFile;
        final boolean isPackage;
        try {
            absPackageFile = new ABSPackageFile(f);
            isPackage = absPackageFile.isABSPackage();
            absPackageFile.close();
        } catch (IOException e) {
            return false;
        }
        return f.getName().endsWith(".jar") && isPackage;
    }

    public static boolean isABSSourceFile(File f) {
        return f.getName().endsWith(".abs") || f.getName().endsWith(".mtvl");
    }

    private static void parseABSSourceFile(java.util.List<CompilationUnit> units, String name, InputStream inputStream, boolean verbose) throws IOException {
        parseABSSourceFile(units, new File(name), new InputStreamReader(inputStream, "UTF-8"), verbose);
    }

    private static void parseABSSourceFile(java.util.List<CompilationUnit> units, File file, boolean verbose) throws IOException {
        parseABSSourceFile(units, file, getUTF8FileReader(file), verbose);
    }

    private static void parseABSSourceFile(java.util.List<CompilationUnit> units, File file, Reader reader, boolean verbose) throws IOException {
        if (verbose) {
            System.out.println("Parsing file " + file.getPath());//getAbsolutePath());
        }
        units.add(parseUnit(file, reader, false));
    }

    protected static void printErrorMessage() {
        System.err.println("\nCompilation failed.");
    }

    protected static void printError(String error) {
        assert error != null;
        System.err.println("\nCompilation failed:\n");
        System.err.println("  " + error);
        System.err.println();
    }

    protected static void printVersion() {
        System.out.println("ABS Tool Suite v"+getVersion());
        System.out.println("Built from git tree " + getGitVersion());
    }


    public static CompilationUnit getStdLib() throws IOException, InternalBackendException {
        InputStream stream = Main.class.getClassLoader().getResourceAsStream(ABS_STD_LIB);
        if (stream == null) {
            // we're running unit tests; try to find the file in the source tree
            stream = Main.class.getClassLoader().getResourceAsStream("abs/lang/abslang.abs");
        }
        if (stream == null) {
            throw new InternalBackendException("Could not find ABS Standard Library");
        }
        return parseUnit(new File(ABS_STD_LIB), new InputStreamReader(stream), false);
    }

    public static void printUsage() {
        printHeader();
        System.out.println(""
                + "Usage: java Main [backend] [options] <absfiles>\n"
                + "\n  <absfiles>     ABS files/directories/packages to parse\n"
                + "\nAvailable backends:\n"
                + "  -maude         generate Maude code\n"
                + "  -java          generate Java code\n"
                + "  -erlang        generate Erlang code\n"
                + "  -haskell       generate Haskell code\n" // this is just for help printing; the execution of the compiler is done by the bash scipt absc
                + "  -prolog        generate Prolog\n"
                + "  -prettyprint   pretty-print ABS code\n\n"
                + "Common options:\n"
                + "  -version       print version\n"
                + "  -product=<PID> build given product by applying deltas (PID is the product ID)\n"
                + "  -checkspl      Check the SPL for errors\n"
                + "  -notypecheck   disable typechecking\n"
                + "  -loctypes      enable location type checking\n"
                + "  -locdefault=<loctype> \n"
                + "                 sets the default location type to <loctype>\n"
                + "                 where <loctype> in " + Arrays.toString(LocationType.ALLUSERTYPES) + "\n"
                + "  -locscope=<scope> \n"
                + "                 sets the location aliasing scope to <scope>\n"
                + "                 where <scope> in " + Arrays.toString(
            LocationTypeInferrerExtension.LocationTypingPrecision.values()) + "\n"
                + "  -solve         solve constraint satisfaction problem (CSP) for the feature\n"
                + "                 model and print a solution\n"
                + "  -solveall      print ALL solutions for the CSP\n"
                + "  -solveWith=<PID>\n"
                + "                 solve CSP by finding a product that includes PID.\n"
                + "  -min=<var>     minimise variable <var> when solving the CSP for the feature\n"
                + "                 model\n"
                + "  -max=<var>     maximise variable <var> when solving the CSP for the feature\n"
                + "                 model\n"
                + "  -maxProduct    print the solution that has the most number of features\n"
                + "  -minWith=<PID> \n"
                + "                 solve CSP by finding a solution that tries to include PID\n"
                + "                 with minimum number of changes.\n"
                + "  -nsol          count the number of solutions\n"
                + "  -noattr        ignore the attributes\n"
                + "  -check=<PID>   check satisfiability of a product with name PID\n"
                + "  -h             print this message\n"
                + "\nDiagnostic options:\n"
                + "  -v             verbose output\n"
                + "  -debug         print stacktrace for crashes during compilation\n"
                + "  -dump          dump AST to standard output\n");
        MaudeCompiler.printUsage();
        JavaBackend.printUsage();
        ErlangBackend.printUsage();
        PrologBackend.printUsage();
        CoreAbsBackend.printUsage();
        PrettyPrinterBackEnd.printUsage();
        OutlinePrinterBackEnd.printUsage();
    }

    protected static void printHeader() {

        String[] header = new String[] {
                "The ABS Compiler" + " v" + getVersion(),
                "Copyright (c) 2009-2013,    The HATS Consortium",
                "Copyright (c) 2013-2016,    The Envisage Project",
        "http://www.abs-models.org/" };

        int maxlength = header[2].length();
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
        if (getGitVersion().endsWith("dirty")) {
            System.out.println("This version of the compiler was created from repository version");
            System.out.println("  " + getGitVersion());
            System.out.println("with uncommitted changes.  Repeatable simulation cannot be guaranteed.");
        } else {
            System.out.println("For repeatable simulations, insert the following comment into the model:");
            System.out.println("// Compiled with git version " + getGitVersion());
        }
        System.out.println();
    }

    public static String getVersion() {
        String version = Main.class.getPackage().getImplementationVersion();
        if (version == null)
            return "HEAD";
        else
            return version;
    }


    public static String getGitVersion() {
        String version = Main.class.getPackage().getSpecificationVersion();
        if (version == null)
            return "HEAD-dirty";
        else
            return version;
    }


    private static BufferedReader getUTF8FileReader(File file) throws UnsupportedEncodingException, FileNotFoundException {
        return new BufferedReader(new InputStreamReader(new FileInputStream(file), "UTF-8"));
    }

    public static Model parse(File file, InputStream stream) throws IOException, InternalBackendException {
        return parse(file, new BufferedReader(new InputStreamReader(stream)));
    }

    public static Model parse(File file, Reader reader) throws IOException, InternalBackendException  {
	List<CompilationUnit> units = new List<>();
	// Note that the unit tests are sensitive to the order in
	// which the compilation units are added to the result.

	// TODO: switch the order of the two next lines, change all
	// freshly-broken unit tests to use `Model.lookup()' instead
	// of positional tree-walking
	units.add(getStdLib());
	units.add(parseUnit(file, reader, false));
	return new Model(units);
    }

    /**
     * Parse the content of `reader` into a CompilationUnit.
     *
     * @param file The filename of the input stream, or null
     * @param reader The stream to parse
     * @param raiseExceptions Raise parse errors as exceptions if true
     * @return The parsed content of `reader`, or an empty CompilationUnit with parse error information
     * @throws IOException
     */
    public static CompilationUnit parseUnit(File file, Reader reader, boolean raiseExceptions)
	throws IOException
    {
	try {
	    SyntaxErrorCollector errorlistener = new SyntaxErrorCollector(file, raiseExceptions);
	    ANTLRInputStream input = new ANTLRInputStream(reader);
	    ABSLexer lexer = new ABSLexer(input);
	    lexer.removeErrorListeners();
	    lexer.addErrorListener(errorlistener);
	    CommonTokenStream tokens = new CommonTokenStream(lexer);
	    ABSParser aparser = new ABSParser(tokens);
	    aparser.removeErrorListeners();
	    aparser.addErrorListener(errorlistener);
	    ParseTree tree = aparser.goal();
	    if (errorlistener.parserErrors.isEmpty()) {
		ParseTreeWalker walker = new ParseTreeWalker();
		CreateJastAddASTListener l = new CreateJastAddASTListener(file);
		walker.walk(l, tree);
		CompilationUnit u
		    = new ASTPreProcessor().preprocess(l.getCompilationUnit());
		for (ModuleDecl d : u.getModuleDecls()) {
		    if (!Constants.STDLIB_NAME.equals(d.getName())) {
			boolean needsImport = true;
			for (Import i : d.getImports()) {
			    if (i instanceof StarImport
				&& ((StarImport)i).getModuleName().equals(Constants.STDLIB_NAME))
				needsImport = false;
			    else if (i instanceof FromImport
				     && ((FromImport)i).getModuleName().equals(Constants.STDLIB_NAME))
				needsImport = false;
			}
			if (needsImport) {
			    d.getImports().add(new StarImport(Constants.STDLIB_NAME));
			}
		    }
		}
		return u;
	    } else {
		String path = "<unknown path>";
		if (file != null) path = file.getPath();
		@SuppressWarnings("rawtypes")
		    CompilationUnit u = new CompilationUnit(path,new List(),new List(),new List(),new Opt(),new List(),new List(),new List());
		u.setParserErrors(errorlistener.parserErrors);
		return u;
	    }
	} finally {
	    reader.close();
	}
    }

    public static Model parseString(String s) throws Exception {
        return parse(null, new StringReader(s));
    }

}
