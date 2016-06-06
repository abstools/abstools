/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.prolog;

import java.io.*;
import java.util.*;
import java.util.List;

import abs.common.NotImplementedYetException;
import abs.frontend.ast.*;
import abs.frontend.parser.Main;

public class PrologBackend extends Main {

    private File destDir = new File(".");
    protected File outFile;
    protected PrintStream outStream;
    private String outFilename = "abs.pl";
    private Model model;
    private ReachabilityInformation reachInfo = null;
    ArrayList<ASTNode<?>> entries = null;

    public static int awaitId = 0;
    public static boolean entriesMode = false;
    
    public static void main(final String... args) {
        runFromShell(args);
    }
    
    public static void runFromShell(final String[] args){
        awaitId = 0;
        PrologBackend prologBE = null;
        try {
            prologBE = new PrologBackend(args);
            prologBE.generateProlog();
            if (Arrays.asList(args).contains("-v"))
                System.out.println("ABS file parsed to Prolog terms in " + prologBE.outFile.getAbsolutePath());
        } catch (NotImplementedYetException e) {
            System.err.println(e.getMessage());
        } catch (Exception e) {
            if (Arrays.asList(args).contains("-v")) {
                System.err.println("An error occurred during compilation:\n" + e.getMessage());
                e.printStackTrace();
            }
            //System.exit(1);
        } finally {
            if (prologBE != null && prologBE.outStream != null) prologBE.outStream.close();
        }
    }
    
    public static void runFromPlugin(Model m,String dir,String fn,ArrayList<ASTNode<?>> entries){
        awaitId = 0;
        PrologBackend prologBE = null;
        try {
            prologBE = new PrologBackend(m,dir,fn,entries);
            prologBE.generateProlog();
        } catch (Exception e) {
            System.err.println("Error in Prolog backend: " + e.getMessage());
        } finally {
            if (prologBE != null && prologBE.outStream != null) prologBE.outStream.close();
        }
    }
    
    // This is the constructor used from runFromShell
    public PrologBackend(String[] args) throws Exception {
        // This parses the args and the ABS program producing the AST whose root is model
        model = parse(args); 
        if (model.hasParserErrors() || model.hasErrors() || model.hasTypeErrors())
            printParserErrorAndExit();
        initOutStreamEtc();
    }
    
    // This is the constructor used from runFromPlugin
    public PrologBackend(Model m,String dir,String fn,ArrayList<ASTNode<?>> es) throws Exception {
        model = m;
        destDir = new File(dir);
        entries = es;
        initOutStreamEtc();
    }
    
    private void initOutStreamEtc() throws Exception {
        destDir.mkdirs();
        if (!destDir.exists()) {
            System.err.println("Destination directory " + destDir.getAbsolutePath() + " does not exist!");
            System.exit(1);
        }
        if (!destDir.canWrite()) {
            System.err.println("Destination directory " + destDir.getAbsolutePath() + " cannot be written to!");
            System.exit(1);
        }
        if (verbose)
            printAST(model, 0);
        outFile = new File(destDir, outFilename);
        // destDir and outFilename are initialized either in the constructor or in parseArgs, 
        // which is called from parse
        outStream = new PrintStream(new BufferedOutputStream(new FileOutputStream(outFile)));
    }
    
    private void generateProlog(){
        if (entries != null){ // mode with entries
            entriesMode = true;
            collectReachableCode(entries);
        }
        
        //print discontiguous predicate to avoid warnings when loading the file
        outStream.print(":-discontiguous mainBlock/3,def/6, data/3,methodImpl/5,"+
        "module/1, starImport/1,type/2,interface/3,class/3.\n");
        
        model.generateProlog(outStream,reachInfo);
    }

    private void collectReachableCode(ArrayList<ASTNode<?>> entries) {
        reachInfo = new ReachabilityInformation(entries);
     
        while (reachInfo.changed()) {
           try {
               model.collectReachableCode(reachInfo);
           } catch (Exception e){
               e.printStackTrace();
           }          
        }
        //System.out.println(reachInfo.toString());  
    }

    public List<String> parseArgs(String[] args) {
        List<String> restArgs = super.parseArgs(args);
        List<String> remainingArgs = new ArrayList<String>();

        for (int i = 0; i < restArgs.size(); i++) {
            String arg = restArgs.get(i);
            if (arg.equals("-prolog")) {
                // nothing to do
            } else if (arg.equals("-d")) {
                i++;
                if (i == restArgs.size()) {
                    System.err.println("Please provide a destination directory");
                    System.exit(1);
                } else {
                    destDir = new File(args[i]);
                }
            } else if (arg.equals("-fn")) {
                i++;
                if (i == restArgs.size()) {
                    System.err.println("Please provide a file name");
                    System.exit(1);
                } else {
                    outFilename = args[i];
                }
            } else {
                remainingArgs.add(arg);
            }
        }
        return remainingArgs;
    }

    protected void printUsage() {
        super.printUsage();
        System.out.println("Prolog Backend:");
        System.out.println("  -d <dir>     generate files to <dir>");
        System.out.println("  -fn <dir>    output file name");
    }
    
    private void printAST(ASTNode<?> ast, int level) {
        if (ast != null) {
            printTab(level);
            System.out.println(level + ":" + ast.getClass().getName() + "@" + ast.getId());
            int n = ast.getNumChild();
            for (int i = 0; i < n; i++)
                printAST(ast.getChild(i), level + 1);
        }
    }

    private void printTab(int n) {
        for (int i = 0; i < n; i++)
            System.out.print("    ");
    }
    
    // Auxiliary methods

    public static String initialToUpperCase(String s){
        char[] cs = s.toCharArray();
        cs[0] = Character.toUpperCase(cs[0]);
        return new String(cs);
    }
    
    public static String varTransform(String s){
        return "l('"+s+"')";
    }

    public static String fieldTransform(String s){
        return "field('"+s+"')";
    }
    public static String strTransform(String s){
        return "str('"+s+"')";
    }

    public static String quote(String s){
        return "'" + s + "'";
    }
}
