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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.jar.JarEntry;

import com.google.inject.Injector;

import org.abs_models.Absc;
import org.abs_models.backend.autodeploy.Tester;
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
import org.abs_models.frontend.ast.CompilationUnit;
import org.abs_models.frontend.ast.DataConstructor;
import org.abs_models.frontend.ast.DataConstructorExp;
import org.abs_models.frontend.ast.DataTypeDecl;
import org.abs_models.frontend.ast.Decl;
import org.abs_models.frontend.ast.ExpFunctionDef;
import org.abs_models.frontend.ast.Feature;
import org.abs_models.frontend.ast.FunctionDecl;
import org.abs_models.frontend.ast.List;
import org.abs_models.frontend.ast.Model;
import org.abs_models.frontend.ast.ModuleDecl;
import org.abs_models.frontend.ast.ProductDecl;
import org.abs_models.frontend.ast.ProductLine;
import org.abs_models.frontend.ast.StringLiteral;
import org.abs_models.frontend.delta.DeltaModellingException;
import org.abs_models.frontend.typechecker.locationtypes.LocationType;
import org.abs_models.frontend.typechecker.locationtypes.LocationTypeExtension;
import org.abs_models.frontend.typechecker.locationtypes.LocationTypeInferenceExtension;
import org.abs_models.xtext.AbsStandaloneSetup;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.xtext.diagnostics.Severity;
import org.eclipse.xtext.resource.XtextResource;
import org.eclipse.xtext.resource.XtextResourceSet;
import org.eclipse.xtext.util.CancelIndicator;
import org.eclipse.xtext.validation.IResourceValidator;
import org.eclipse.xtext.validation.Issue;

/**
 * @author rudi
 *
 */
public class Main {

    // Do this only once per application.  Public because the tests need
    // access
    public static final Injector absinjector = new AbsStandaloneSetup().createInjectorAndDoEMFRegistration();

    public static final String ABS_STD_LIB = "abs/lang/abslang.abs";
    public static final String UNKNOWN_FILENAME = "<unknown file>";
    public Absc arguments = new Absc(); // tests often create a random Main object, need to initialize this


    /**
     * Legacy entry point for old scripts -- use org.abs_models.Absc
     * main method instead.
     */
    public static void main(final String... args)  {
        Absc.main(args);
    }

    public int mainMethod(Absc arguments) {
        int result = 0;
        boolean done = false;
        this.arguments = arguments;
        try {
            if (arguments.backend != null) {
                if (arguments.backend.maude) {
                    result = Math.max(result, MaudeCompiler.doMain(arguments));
                    done = true;
                }
                if(arguments.backend.java) {
                    result = Math.max(result, JavaBackend.doMain(arguments));
                    done = true;
                }
                if (arguments.backend.erlang) {
                    result = Math.max(result, ErlangBackend.doMain(arguments));
                    done = true;
                }
                if (arguments.backend.prolog) {
                    result = Math.max(result, PrologBackend.doMain(arguments));
                    done = true;
                }
                if (arguments.backend.coreabs) {
                    result = Math.max(result, CoreAbsBackend.doMain(arguments));
                    done = true;
                }
                if (arguments.backend.json) {
                    result = Math.max(result, Tester.doMain(arguments));
                    done = true;
                }
                if (arguments.backend.prettyprint) {
                    result = Math.max(result, PrettyPrinterBackEnd.doMain(arguments));
                    done = true;
                }
                if (arguments.backend.outline) {
                    result = Math.max(result, OutlinePrinterBackEnd.doMain(arguments));
                    done = true;
                }
                if (arguments.backend.dumpProducts) {
                    Model m = parse(arguments.files);
                    if (m == null || m.hasParserErrors()) {
                        // parse should have already printed errors
                        result = Math.max(result, 1);
                    } else {
                        Iterator<ProductDecl> pi = m.getProductDecls().iterator();
                        while (pi.hasNext()) {
                            System.out.print(pi.next().getName());
                            if (pi.hasNext()) System.out.print(' ');
                        }
                    }
                    done = true;
                }
            }
            if (!done) {
                // no backend selected, just do type-checking
                Model m = parse(arguments.files);
                if (m == null || m.hasParserErrors() || m.hasErrors() || m.hasTypeErrors()) {
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
        }
        return remainingArgs;
    }

    /**
     * Entry point for parsing files.  This method uses the `arguments` field
     * for additional parameters.  In case of parse errors, returns null and
     * prints diagnostic messages to stdout.
     *
     * @param args - a list of File 
     * @return A Model instance, or null in case of errors.
     * @throws IOException
     * @throws DeltaModellingException
     * @throws WrongProgramArgumentException
     * @throws InternalBackendException
     */
    //
    public Model parse(final java.util.List<File> args) throws IOException, DeltaModellingException, WrongProgramArgumentException, InternalBackendException {
        Model m = parseFiles(this.arguments.verbose, args);
        if (m != null) analyzeFlattenAndRewriteModel(m);
        return m;
    }

    /**
     *
     * @param verbose controls whether to print diagnostic messages
     * @param fileNames the files to parse into a Model
     * @return An instance of Model, or null in case of errors in the source files.
     * @throws IOException
     * @throws InternalBackendException
     */
    private static Model parseFiles(boolean verbose, final java.util.List<File> fileNames) throws IOException, InternalBackendException {
        if (fileNames.isEmpty()) {
            throw new IllegalArgumentException("Please provide at least one input file");
        }

        // TODO parse not only files but also package files

        // TODO document abs package files (this involves finding out what they do)

                // https://typefox.io/how-and-why-use-xtext-without-the-ide
        // (outdated) and
        // https://stackoverflow.com/questions/47110291/xtext-standalone-and-validation
        // and
        // https://www.eclipse.org/Xtext/documentation/302_configuration.html#dependency-injection

        // obtain a resourceset from the injector
        XtextResourceSet resourceSet = absinjector.getInstance(XtextResourceSet.class);

        for (File file : fileNames) {
            resourceSet.createResource(org.eclipse.emf.common.util.URI.createFileURI(file.toString()))
                .load(null);
        }
        // load the standard library
        resourceSet.createResource(org.eclipse.emf.common.util.URI.createURI(Main.class.getClassLoader().getResource(ABS_STD_LIB).toString()))
            .load(null);
        boolean hasErrors = false;
        for (Resource r : resourceSet.getResources()) {
            IResourceValidator validator = ((XtextResource)r).getResourceServiceProvider().getResourceValidator();
            java.util.List<Issue> issues = validator.validate(r, org.eclipse.xtext.validation.CheckMode.ALL, CancelIndicator.NullImpl);
            for (Issue issue : issues) {
                if (issue.getSeverity() == Severity.ERROR) hasErrors = true;
                String filename = issue.getUriToProblem().toFileString();
                if (filename == null) filename = UNKNOWN_FILENAME;
                System.out.println(filename
                                   + ":" + issue.getLineNumber()
                                   // we need zero-based column numbers
                                   + ":" + (issue.getColumn() - 1)
                                   + ":" + issue.getMessage()
                                   );
            }
        }
        Model result = null;
        if (!hasErrors) {
            result = XtextToJastAdd.fromResourceSet(resourceSet);
        } else {
            System.out.println("\nNote that the ABS syntax has changed after release 1.8.2!");
            System.out.println("For the conditional expression, write ‘when .. then .. else ..’\ninstead of ‘if .. then .. else ..’.\nFor the pattern-matching statement, write ‘switch’ instead of ‘case’.\n(For the pattern-matching expression, write ‘case’ as before.)");
        }
        return result;

        // ------------------------- old code follows -------------------------

        // java.util.List<CompilationUnit> units = new ArrayList<>();

        // for (File f : fileNames) {
        //     if (!f.canRead()) {
        //         throw new IllegalArgumentException("File "+f+" cannot be read");
        //     }

        //     if (!f.isDirectory() && !isABSSourceFile(f) && !isABSPackageFile(f)) {
        //         throw new IllegalArgumentException("File "+f+" is not a legal ABS file");
        //     }
        // }

        // for (File f : fileNames) {
        //     parseFileOrDirectory(units, f, verbose);
        // }

	// units.add(getStdLib());

        // List<CompilationUnit> unitList = new List<>();
        // for (CompilationUnit u : units) {
        //     unitList.add(u);
        // }

        // Model m = new Model(unitList);
        // return m;
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
    private void analyzeFlattenAndRewriteModel(Model m) throws WrongProgramArgumentException, DeltaModellingException, FileNotFoundException {
        m.verbose = arguments.verbose;
        m.debug = arguments.debug;
        m.doAACrewrite = !arguments.prettyprint_keepsugar;
        m.doForEachRewrite = !arguments.prettyprint_keepsugar;

        if (m.hasParserErrors()) {
            System.err.println("Syntactic errors: " + m.getParserErrors().size());
            for (ParserError e : m.getParserErrors()) {
                System.err.println(e.getHelpMessage());
                System.err.flush();
            }
            return;
        }

        m.evaluateAllProductDeclarations(); // resolve ProductExpressions to simple sets of features
        rewriteModel(m, arguments.product);
        m.flattenTraitOnly();
        m.collapseTraitModifiers();

        m.expandPartialFunctions();
        m.expandForeachLoops();
        m.expandAwaitAsyncCalls();

        if (arguments.product != null) {
            // apply deltas that correspond to arguments.productproduct
            if (arguments.notypecheck) {
                m.flattenForProductUnsafe(arguments.product);
            } else {
                m.flattenForProduct(arguments.product);
            }
        }

        if (arguments.dump) {
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
        }
    }

    /**
     * Perform various rewrites that cannot be done in JastAdd.
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
     * @param productname The name of the product or null.
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
            if (modProductline == null) {
                throw new WrongProgramArgumentException("Internal error: did not find module " + Constants.PL_NAME + "(should have been defined in the abslang.abs standard library)");
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
            featureDecl.setDataConstructorList(new List<>());
            for (Feature f : pl.getFeatures()) {
                // TODO: when/if we incorporate feature parameters into the
                // productline feature declarations (as we should), we need to
                // adjust the DataConstructor arguments here.
                featureDecl.addDataConstructorNoTransform(new DataConstructor(f.getName(), new List<>()));
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

    private void typeCheckModel(Model m) {
        if (!arguments.notypecheck) {
            if (arguments.verbose)
                System.out.println("Typechecking Model...");

            registerLocationTypeChecking(m);
            SemanticConditionList typeerrors = m.typeCheck();
            for (SemanticCondition se : typeerrors) {
                System.err.println(se.getHelpMessage());
            }
        }
    }

    private void registerLocationTypeChecking(Model m) {
        if (arguments.locationTypeInferenceEnabled) {
            if (arguments.verbose)
                System.out.println("Registering Location Type Checking...");
            LocationTypeInferenceExtension ltie = new LocationTypeInferenceExtension(m);

            if (arguments.verbose) {
                // lte.enableStatistics();
            }
            if (arguments.debug) {
                // ltie.enableDebugOutput();
            }
            if (arguments.defaultLocationType != null) {
                ltie.setDefaultType(arguments.defaultLocationType);
            }
            /*if (arguments.locationTypeScope != null) {
                ltie.setLocationTypingPrecision(arguments.locationTypeScope);
            }*/
            m.registerTypeSystemExtension(ltie);
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
        BufferedReader reader = new BufferedReader(new InputStreamReader(new FileInputStream(file), "UTF-8"));
        parseABSSourceFile(units, file, reader, verbose);
    }

    private static void parseABSSourceFile(java.util.List<CompilationUnit> units, File file, Reader reader, boolean verbose) throws IOException {
        if (verbose) {
            System.out.println("Parsing file " + file.getPath());//getAbsolutePath());
        }
        units.add(parseUnit(file, reader));
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
        System.out.println("ABS Tool Suite "+getVersion());
        System.out.println("Built from git tree " + getGitVersion());
    }


    private static CompilationUnit getStdLib() throws IOException, InternalBackendException {
        InputStream stream = Main.class.getClassLoader().getResourceAsStream(ABS_STD_LIB);
        if (stream == null) {
            // we're running unit tests; try to find the file in the source tree
            stream = Main.class.getClassLoader().getResourceAsStream("abs/lang/abslang.abs");
        }
        if (stream == null) {
            throw new InternalBackendException("Could not find ABS Standard Library");
        }
        return parseUnit(new File(ABS_STD_LIB), new InputStreamReader(stream));
    }

    @Deprecated
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
                + "  -notypecheck   disable typechecking\n"
                + "  -loctypes      enable location type checking\n"
                + "  -locdefault=<loctype> \n"
                + "                 sets the default location type to <loctype>\n"
                + "                 where <loctype> in " + Arrays.toString(LocationType.ALL_USER_TYPES) + "\n"
            //    + "  -locscope=<scope> \n"
            //    + "                 sets the location aliasing scope to <scope>\n"
            //    + "                 where <scope> in " + Arrays.toString(
            // LocationTypeInferrerExtension.LocationTypingPrecision.values()) + "\n"
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
);
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

    // Low-level entry point kept around for the benefit of the unit tests,
    // who often need to parse from a string
    public static Model parse(File file, Reader reader) throws IOException, InternalBackendException  {
	List<CompilationUnit> units = new List<>();
	// Note that the unit tests are sensitive to the order in
	// which the compilation units are added to the result.

	// TODO: switch the order of the two next lines, change all
	// freshly-broken unit tests to use `Model.lookup()' instead
	// of positional tree-walking
	units.add(getStdLib());
	units.add(parseUnit(file, reader));
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
    private static CompilationUnit parseUnit(File file, Reader reader)
	throws IOException
    { return null; }
    // {
    //     try {
    //         SyntaxErrorCollector errorlistener = new SyntaxErrorCollector(file);
    //         ANTLRInputStream input = new ANTLRInputStream(reader);
    //         ABSLexer lexer = new ABSLexer(input);
    //         lexer.removeErrorListeners();
    //         lexer.addErrorListener(errorlistener);
    //         CommonTokenStream tokens = new CommonTokenStream(lexer);
    //         ABSParser aparser = new ABSParser(tokens);
    //         aparser.removeErrorListeners();
    //         aparser.addErrorListener(errorlistener);
    //         ParseTree tree = aparser.goal();
    //         if (errorlistener.parserErrors.isEmpty()) {
    //     	ParseTreeWalker walker = new ParseTreeWalker();
    //     	CreateJastAddASTListener l = new CreateJastAddASTListener(file);
    //     	walker.walk(l, tree);
    //     	CompilationUnit u
    //     	    = new ASTPreProcessor().preprocess(l.getCompilationUnit());
    //     	return u;
    //        } else {
    //             String path = "<unknown path>";
    //             if (file != null) path = file.getPath();
    //             CompilationUnit u = new CompilationUnit();
    //             u.setName(path);
    //             u.setParserErrors(errorlistener.parserErrors);
    //             return u;
    //         }
    //     	String path = "<unknown path>";
    //     	if (file != null) path = file.getPath();
    //     	@SuppressWarnings("rawtypes")
    //     	    CompilationUnit u = new CompilationUnit(path,new List(),new List(),new List(),new Opt(),new List(),new List(),new List());
    //     	u.setParserErrors(errorlistener.parserErrors);
    //     	return u;
    //         }
    //     } finally {
    //         reader.close();
    //     }
    // }

}
