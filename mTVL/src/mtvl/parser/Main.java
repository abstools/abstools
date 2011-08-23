/*
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */

//$Id: Main.java 2032 2010-11-17 13:33:06Z jose $
// Adapted from: 
//   abs/frontend/parser/Main.java
//   6015 2010-09-22 13:58:36Z jschaefer

package mtvl.parser;


import java.io.*;
import java.util.ArrayList;

import java.util.Enumeration;
import java.util.Map;
import java.util.HashMap;
import java.util.jar.JarEntry;

import abs.backend.java.lib.runtime.ABSAndGuard;
import abs.frontend.parser.ABSPackageFile;
import beaver.Parser;
import mtvl.ast.CompilationUnit;
import mtvl.ast.List;
import mtvl.ast.Model;
//import mtvl.ast.ModuleDecl;
//import mtvl.ast.StarImport;
import mtvl.parser.MTVLParser;
import mtvl.parser.MTVLScanner;
import mtvl.analyser.SemanticError;
import mtvl.analyser.SemanticErrorList;
import mtvl.analyser.ChocoSolver;
//import abs.frontend.parser.SyntaxError;
import abs.frontend.parser.ParseException;
// import fsl.frontend.parser.FSLParser;
// import fsl.frontend.parser.FSLScanner;
import abs.frontend.parser.ABSParser;
import abs.frontend.parser.ABSScanner;


public class Main {

    protected boolean verbose = false ;
    protected boolean solve = false ;
    protected boolean check = false ;

    public static void main(final String[] args) throws Exception {
        new Main().parse(args);
    }

    public java.util.List<String> parseArgs(String[] args) throws Exception {
        ArrayList<String> remaindingArgs = new ArrayList<String>();
        
        for (String arg : args) {
            if (arg.equals("-v")) {
                verbose = true;
            } else if (arg.equals("-s")) {
                solve = true;
            } else if (arg.equals("-c")) {
                check = true;
            } else if (arg.equals("-h")) {
                printUsageAndExit();
            } else {
                remaindingArgs.add(arg);   
            }
        }

        return remaindingArgs;      
    }
    
    public Model parse(final String[] args) throws Exception {

        java.util.List<String> files = parseArgs(args);
        
        if (files.isEmpty()) {
            printErrorAndExit("Please provide at least on intput file");
        }
        
        java.util.List<CompilationUnit> units   = new ArrayList<CompilationUnit>();
        java.util.List<abs.frontend.ast.CompilationUnit> fsunits = new ArrayList<abs.frontend.ast.CompilationUnit>();
        //abs.frontend.ast.CompilationUnit fsunit = null;
        //fsunits.add(getStdLib());

                
        boolean checkfst = check;
        String product = null;

        for (String file : files){
            if (file.startsWith("-")) {
                printErrorAndExit("Illegal option "+file);
            }

            if (isABSPackageFile(new File(file))) {
                parseABSPackageFile(fsunits, units, new File(file));
            }
            else try{
                if (checkfst && (product == null))
                  product = file;
                else if (checkfst) {
                  System.out.println("parsing ABS file:  "+file);
//                  fsunit = parseFSUnit(new File(file));
                  fsunits.add(parseFSUnit(new File(file)));
                  checkfst = false;
                }
                else{
                  System.out.println("parsing mTVL file: "+file);
                  units.add(parseUnit(new File(file)));
                }
                
            } catch (FileNotFoundException e1) {
                printErrorAndExit("File not found: " + file);
            } catch (ParseException pex) {
                System.err.println(file + ":" + pex.getError().getHelpMessage());
                System.exit(1);
            } catch (Exception e1) {
                // Catch-all
                System.err.println("Compilation of " + file +  " failed with Exception");
                System.err.println(e1);
                System.exit(1);
            }
        }

        List<CompilationUnit> unitList =
              new List<CompilationUnit>();
        for (CompilationUnit u : units) {
            unitList.add(u);
        }
        abs.frontend.ast.List<abs.frontend.ast.CompilationUnit> fsunitList =
              new abs.frontend.ast.List<abs.frontend.ast.CompilationUnit>();
        for (abs.frontend.ast.CompilationUnit u : fsunits) {
            fsunitList.add(u);
        }

        Model m = new Model(unitList);
        abs.frontend.ast.Model fsm = new abs.frontend.ast.Model(fsunitList);
        
        // Dump tree for debug
        if (verbose){ 
            System.out.println("### MTVL Result:");
            System.out.println(m);
            m.dumpTree("  ", System.out);
            System.out.println("### free variables:");
            System.out.println("int:  "+m.ints().keySet());
            System.out.println("bool: "+m.bools());
            System.out.println("feat: "+m.features());
            if (check) {
              System.out.println("### FSL Result:");
              System.out.println(fsm);
              fsm.dumpTree("  ", System.out);
           }
        }

        SemanticErrorList errors = m.getErrors();
        int numSemErrs = errors.size();
            
        if (numSemErrs > 0) {
            System.out.println("Semantic errors: " + numSemErrs);
            for (SemanticError error : errors) {
                System.err.println(error.getMsgString());
                System.err.flush();
            }
        } else { // no errors for the MTVL, no semantic error checking in FSL
          if (solve) {
            ChocoSolver s = m.getCSModel(verbose);
            System.out.print(s.resultToString());
          }
          if (check) {
            ChocoSolver s = m.getCSModel(verbose);
            Map<String,Integer> guess = new HashMap<String,Integer>();
            if (fsm.getSolution(product,guess))
              System.out.println("checking solution: "+s.checkSolution(guess,m));
            else {
              System.out.println("Product '"+product+"' not found.");
              if (!product.contains("."))
                System.out.println("Maybe you forgot the module name?");
            }

          }
//           else {
//             ChocoSolver s = m.getCSModel(verbose);
//             Map<String,Integer> guess = new HashMap<String,Integer>();
//             guess.put("Language",1);
//             guess.put("I18n",1);
//             guess.put("English",1);
//             System.out.println("checking solution: "+s.checkSolution(guess));            
//           }
        }
//         } else {
//             if (typecheck) {
//                 SemanticErrorList typeerrors = m.typeCheck();
//                 for (SemanticError se : typeerrors) {
//                     System.err.println(se.getMsgString());
//                 }
//             }
//         }
        
        return m;
    }

    private boolean isABSPackageFile(File f) throws IOException {
       return f.getName().endsWith(".jar") && new ABSPackageFile(f).isABSPackage();
    }

    private void parseABSPackageFile(java.util.List<abs.frontend.ast.CompilationUnit> fsunits, java.util.List<CompilationUnit> units, File file) throws IOException {
        ABSPackageFile jarFile = new ABSPackageFile(file);
        if (!jarFile.isABSPackage())
           return;
        Enumeration<JarEntry> e = jarFile.entries();
        while (e.hasMoreElements()) {
           JarEntry jarEntry = e.nextElement();
            if (!jarEntry.isDirectory()) {
              if (jarEntry.getName().endsWith(".abs")) {
                  parseABSSourceFile(fsunits, "jar:"+file.toURI()+"!/"+jarEntry.getName(), jarFile.getInputStream(jarEntry));
              }
              else if (jarEntry.getName().endsWith(".mtvl")) {
                  parseMTVLSourceFile(units, "jar:" + file.toURI() + "!/" + jarEntry.getName(), jarFile.getInputStream(jarEntry));
              }
            }
        }
    }

    private void parseABSSourceFile(java.util.List<abs.frontend.ast.CompilationUnit> units, String name, InputStream inputStream) throws IOException {
        parseABSSourceFile(units, new File(name), new InputStreamReader(inputStream, "UTF-8"));
    }

    private void parseABSSourceFile(java.util.List<abs.frontend.ast.CompilationUnit> units, File file) throws IOException {
        parseABSSourceFile(units, file, getUTF8FileReader(file));
    }

    private void parseABSSourceFile(java.util.List<abs.frontend.ast.CompilationUnit> units, File file, Reader reader) throws IOException {
        if (verbose)
            System.out.println("Parsing file "+file.getAbsolutePath());
        units.add(parseFSUnit(file, reader));
    }

    private void parseMTVLSourceFile(java.util.List<CompilationUnit> units, String name, InputStream inputStream) throws IOException {
        parseMTVLSourceFile(units, new File(name), new InputStreamReader(inputStream, "UTF-8"));
    }

    private void parseMTVLSourceFile(java.util.List<CompilationUnit> units, File file) throws IOException {
        parseMTVLSourceFile(units, file, getUTF8FileReader(file));
    }

    private void parseMTVLSourceFile(java.util.List<CompilationUnit> units, File file, Reader reader) throws IOException {
        if (verbose)
            System.out.println("Parsing file "+file.getAbsolutePath());
        units.add(parseUnit(file, reader));
    }



    private void printErrorAndExit(String error) {
        System.err.println("\nCompilation failed:\n");
        System.err.println("  "+error);
        System.err.println();
        printUsageAndExit();
    }

    private void printUsageAndExit() {
        printUsage();
        System.exit(1);
    }



    protected void printUsage() {
        System.out.println(
                "*******************************\n"+
                "*       MTVL TOOL SUITE       *\n"+
                "*******************************\n"+
                "Usage: java "+this.getClass().getName()+" [options] [prodname] [absfile] <mtvlfiles>\n\n" +
                "  [prodname]     name of the product to check: mandatory with option -c\n" +
                "  [absfile]      ABS file to parse: mandatory with option -c\n" +
                "  <mtvlfiles>    mTVL files to parse\n\n" +
                "Options:\n"+
                "  -v            verbose output\n" +
                "  -s            solve constraint satisfaction problem\n" +
                "  -c            check satisfiability of a feature selection\n" +
                "  -h            print this message\n");        
    }



    private BufferedReader getUTF8FileReader(File file) throws UnsupportedEncodingException, FileNotFoundException {
        return new BufferedReader(new InputStreamReader(new FileInputStream(file), "UTF-8"));
    }

    public static CompilationUnit parseUnit(File file) throws Exception {
        return parseUnit(file,new FileReader(file));
    }

    public static CompilationUnit parseUnit(File file, Reader reader) throws IOException {
                //Reader reader = new FileReader(file);
        BufferedReader rd = null;
        //Set to true to print source before parsing 
        boolean dumpinput = false;
        if (dumpinput){
            try {
                rd = new BufferedReader(new FileReader(file));
                String line = null;
                int i = 1 ; 
                while ((line = rd.readLine()) != null) {
                    System.out.println(i++ + "\t" + line);
                }
            } catch (IOException x) {
                System.out.flush();
                System.err.println(x);
                System.err.flush();
            } finally {
                if (rd != null) rd.close();
            }
        }

        return parseUnit(file, null, reader); 
    }
    
    public static Model parse(File file) throws Exception {
        List<CompilationUnit> units = new List<CompilationUnit>();
        units.add(parseUnit(file));
        return new Model(units);
    }
    
    public static Model parse(ArrayList<String> fileNames) throws Exception {
        List<CompilationUnit> units = new List<CompilationUnit>();
        for (String filename : fileNames) {
            units.add(parseUnit(new File(filename)));
        }
        return new Model(units);
    }
    
    public static Model parse(File file, String sourceCode, InputStream stream) throws Exception {
        return parse(file, sourceCode, new BufferedReader(new InputStreamReader(stream)));
    }
        
    public static Model parse(File file, String sourceCode, Reader reader) throws Exception {
        List<CompilationUnit> units = new List<CompilationUnit>();
        units.add(parseUnit(file, sourceCode, reader));
        return new Model(units);
    }
    
    public static CompilationUnit parseUnit(File file, String sourceCode, Reader reader) throws IOException {
        try {
            MTVLParser parser = new MTVLParser();
            MTVLScanner scanner = new MTVLScanner(reader);
            parser.setSourceCode(sourceCode);
            parser.setFile(file);
            
            CompilationUnit u = null;

            try {
                u = (CompilationUnit) parser.parse(scanner);
            } catch (Parser.Exception e) {
                u = new CompilationUnit(new mtvl.ast.List(),new mtvl.ast.List());
                //u.setParserErrors(parser.getErrors());
            }
            return u;
        } finally {
            reader.close();
        }
    }


    public static Model parseString(String s) throws Exception {
        return parse(null,s, new StringReader(s));
    }
    
    // Parsing the FSL file
    public static abs.frontend.ast.CompilationUnit parseFSUnit(File file)
    throws IOException {
        return parseFSUnit(file, new FileReader(file));
    }

    public static abs.frontend.ast.CompilationUnit parseFSUnit(File file, Reader reader)
    throws IOException {
        //Reader reader = new FileReader(file);
        BufferedReader rd = null;
        //Set to true to print source before parsing 
        boolean dumpinput = false;
        if (dumpinput){
            try {
                rd = new BufferedReader(new FileReader(file));
                String line = null;
                int i = 1 ; 
                while ((line = rd.readLine()) != null) {
                    System.out.println(i++ + "\t" + line);
                }
            } catch (IOException x) {
                System.out.flush();
                System.err.println(x);
                System.err.flush();
            } finally {
                if (rd != null) rd.close();
            }
        }

        try {
            ABSParser parser = new ABSParser();
            ABSScanner scanner = new ABSScanner(reader);
            parser.setSourceCode(null);
            parser.setFile(file);
            
            abs.frontend.ast.CompilationUnit u = null;
            try {
                u = (abs.frontend.ast.CompilationUnit) parser.parse(scanner);
            } catch (Parser.Exception e) {
                u = new abs.frontend.ast.CompilationUnit(parser.getFileName(), new abs.frontend.ast.List());
                u.setParserErrors(parser.getErrors());
            }
            return u;
        } finally {
            reader.close();
        }
    }
}
