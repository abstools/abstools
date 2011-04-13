/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.parser;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.HashSet;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;

import abs.common.Constants;
import abs.common.WrongProgramArgumentException;
import abs.frontend.analyser.SemanticError;
import abs.frontend.analyser.SemanticErrorList;
import abs.frontend.ast.CompilationUnit;
import abs.frontend.ast.List;
import abs.frontend.ast.Model;
import abs.frontend.ast.ModuleDecl;
import abs.frontend.ast.StarImport;
import abs.frontend.delta.*;
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
    protected String product;
    protected boolean locationTypeStats = false;
    protected LocationTypingPrecision locationTypeScope = null;

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
            printErrorAndExit("Please provide at least one intput file");
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

    private void analyzeModel(Model m) throws WrongProgramArgumentException, ASTNodeNotFoundException {
        if (verbose)
            System.out.println("Analyzing Model...");
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
                    // apply deltas that correspond to given product
                    m.flattenForProduct(product);
                    
                    if (dump)
                        m.dump();
                }
                typeCheckModel(m);
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
                    parseABSSourceFile(units, jarEntry.getName(), jarFile.getInputStream(jarEntry));
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

    private boolean isABSPackageFile(File f) throws IOException {
       return f.getName().endsWith(".jar") && new ABSPackageFile(f).isABSPackage();
    }

    private boolean isABSSourceFile(File f) {
        return f.getName().endsWith(".abs");
    }

    private void parseABSSourceFile(java.util.List<CompilationUnit> units, String name, InputStream inputStream) throws IOException {
        parseABSSourceFile(units, new File(name), new InputStreamReader(inputStream));
    }

    private void parseABSSourceFile(java.util.List<CompilationUnit> units, File file) throws IOException {
        parseABSSourceFile(units, file, new FileReader(file));
    }
    
    private void parseABSSourceFile(java.util.List<CompilationUnit> units, File file, Reader reader) throws IOException {
        if (verbose)
            System.out.println("Parsing file "+file.getAbsolutePath());
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
            System.err.println("Could not found ABS Standard Library");
            System.exit(1);
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

        return parseUnit(file, null, reader);
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
                u = new CompilationUnit(parser.getFileName(), new List());
                u.setParserErrors(parser.getErrors());
            }
            if (stdlib) {
                for (ModuleDecl d : u.getModuleDecls()) {
                    if (!d.getName().equals(Constants.STDLIB_NAME))
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
