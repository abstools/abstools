/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.prolog;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.util.ArrayList;

import org.abs_models.Absc;
import org.abs_models.backend.common.InternalBackendException;
import org.abs_models.common.NotImplementedYetException;
import org.abs_models.common.WrongProgramArgumentException;
import org.abs_models.frontend.ast.ASTNode;
import org.abs_models.frontend.ast.Model;
import org.abs_models.frontend.delta.DeltaModellingException;
import org.abs_models.frontend.parser.Main;

public class PrologBackend extends Main {

    protected File outFile;
    protected PrintStream outStream;
    private String defaultOutFilename = "abs.pl";
    private Model model;
    private ReachabilityInformation reachInfo = null;
    ArrayList<ASTNode<?>> entries = null;

    public static int awaitId = 0;
    public static boolean entriesMode = false;

    public static int doMain(Absc arguments) {
        int result = 0;
        awaitId = 0;
        PrologBackend prologBE = null;
        try {
            prologBE = new PrologBackend();
            prologBE.arguments = arguments;
            result = prologBE.generateProlog();
            if (arguments.verbose && arguments.outputfile != null) {
                System.out.println("ABS file parsed to Prolog terms in " + prologBE.outFile.getAbsolutePath());
            }
        } catch (NotImplementedYetException e) {
            System.err.println(e.getMessage());
            result = 1;
        } catch (Exception e) {
            System.err.println("An error occurred during compilation:\n" + e.getMessage());
            if (arguments.debug) {
                e.printStackTrace();
            }
            result = 1;
        } finally {
            if (prologBE != null && prologBE.outStream != null) prologBE.outStream.close();
        }
        return result;
    }

    private int generateProlog() throws DeltaModellingException, IOException, WrongProgramArgumentException, InternalBackendException{
        model = parse(arguments.files);
        int result = 0;
        if (model.hasParserErrors() || model.hasErrors() || model.hasTypeErrors()) {
            printErrorMessage();
            return 1;
        }
        if (arguments.verbose) {
            printAST(model, 0);
        }
        outFile = arguments.outputfile;
        if (outFile == null) {
            outFile = new File(arguments.destDir, defaultOutFilename);
        }
        outStream = new PrintStream(new BufferedOutputStream(new FileOutputStream(outFile)));
        if (entries != null){ // mode with entries
            entriesMode = true;
            collectReachableCode(entries);
        }

        //print discontiguous predicate to avoid warnings when loading the file
        outStream.print(":-discontiguous mainBlock/3,def/6, data/3,methodImpl/5,"+
        "module/1, starImport/1,type/2,interface/3,class/3.\n");

        model.generateProlog(outStream,reachInfo);
        return result;
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
