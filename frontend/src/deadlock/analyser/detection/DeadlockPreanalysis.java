/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package deadlock.analyser.detection;

import java.util.HashMap;

import abs.frontend.ast.ASTNode;
import abs.frontend.ast.AwaitStmt;
import abs.frontend.ast.CompilationUnit;
import abs.frontend.ast.DataTypeUse;
import abs.frontend.ast.FieldDecl;
import abs.frontend.ast.GetExp;
import abs.frontend.ast.InterfaceTypeUse;
import abs.frontend.ast.Model;
import abs.frontend.ast.VarUse;
import deadlock.analyser.AnalyserLog;

/**
 * A class to pre-process the program in order to evaluate whether the program 
 * requires a deadlock analysis.
 * @author groman
 *
 */
public class DeadlockPreanalysis {
    /**
     * The model of the program
     */
    private Model model;
    /**
     * Log object
     */
    private AnalyserLog log;

    /**
     * Get expressions found and their await statement (if exists)
     */
    private HashMap<GetExp,AwaitStmt> getExpressions; 
    
    /**
     * DataType Field declarations found in the program and if they might have a future value 
     */
    private HashMap<FieldDecl,Boolean> dataFieldDecls; 

    /**
     * Stores the model
     * @param model The model of the program
     */
    public DeadlockPreanalysis(Model model) {
        this.model = model;
        log = new AnalyserLog(); 
        getExpressions = new HashMap<GetExp,AwaitStmt> (); 
        dataFieldDecls = new HashMap<FieldDecl,Boolean>(); 
    }

    /** 
     * Applies the deadlock preanalysis 
     */
    public void analyzeModel () {
        try {
            //CompilationUnit cu = model.getCompilationUnits().getChild(0); 
            for (CompilationUnit cu : model.getCompilationUnits()) {
                computeInstructions(cu,0);
                if (cu.hasMainBlock()) {
                    System.out.println("Processing main block" + cu.getName());
                    computeInstructions(cu.getMainBlock(),0);
                }
            }
            System.out.println(getExpressions);
            System.out.println(this);
            
            
            
        }
        catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * Computes all instructions (recursively) searching get statements and field declarations. 
     * Relates get statements to previous "await" statements on the same variable.
     * Process all fields evaluating their types and detecting if they can contain a future variable.
     * @param node Node to be evaluated
     * @param level Level in the AST
     */
    private void computeInstructions(ASTNode<?> node, int level) {
        // We save all get instructions
        if (node instanceof GetExp && !getExpressions.containsKey(node)) {
            getExpressions.put((GetExp)node, null);
        }

        // We evaluate all field declarations
        if (node instanceof FieldDecl) {
            processFieldDecl ((FieldDecl)node); 
        }
        
        // Search recursively
        for(int i = 0; i < node.getNumChild(); i++) {
            ASTNode<?> child =  node.getChild(i);

            // If we find an await we search for its get
            if (node.getChild(i) instanceof AwaitStmt && node.getNumChild() > i) {
                //processAwait((AwaitStmt)node.getChild(i), node.getChild(i+1)); 
                processAwait((AwaitStmt)node.getChild(i), i, node);
            }
            computeInstructions(child, level + 1);
        }
    }

    private void processAwait (AwaitStmt await, int position, ASTNode<?> node) {
        String varAwait = null; 
        if (await.getGuard().getChild(0) instanceof VarUse) {
            varAwait = ((VarUse)await.getGuard().getChild(0)).getName();
        }

        int getPos = position + 1;
        GetExp getInst = null; 
        if (varAwait != null) {
            while (getPos < node.getNumChild() && 
                    getInst == null) {
                getInst = lookforGetOperation(await,node.getChild(getPos),varAwait); 
                if (getInst == null) {
                    getPos ++; 
                }
            }
        }
//        System.out.println("PROCESANDO " + position + " " + getPos);

        boolean used = false; 
        for (int i = position+1; i < getPos && !used; i ++) {
            used = isVarUsed(node.getChild(i),varAwait);
        }       
        if (used && getInst != null) {
            getExpressions.put((GetExp)getInst, null);
        }

    }

    private boolean isVarUsed (ASTNode<?> node, String var) {
        if (node instanceof VarUse) {
            VarUse vUse = (VarUse) node; 
            if (var.equals(vUse.getName())) {
                return true; 
            }
        }
        boolean used = false; 
        for (int i = 0; i < node.getNumChild() && !used ; i++) {
            used = isVarUsed (node.getChild(i), var); 
        }
        return used; 
    }


//    private void processAwait (AwaitStmt await, ASTNode<?> next) {
//        if (await.getGuard().getChild(0) instanceof VarUse) {
//            String varAwait = ((VarUse)await.getGuard().getChild(0)).getName();
//            lookforGetOperation(await,next,varAwait);
//        }
//    }

    private GetExp lookforGetOperation (AwaitStmt await, ASTNode<?> node, String varName) {
        GetExp result = null; 
        if (node instanceof GetExp) {
            GetExp get = (GetExp)node;   
            if (varName.equals(((VarUse)get.getChild(0)).getName())) {
                result = (GetExp)node; 
                getExpressions.put((GetExp)node, await);
            }
        }
        for (int i = 0; i < node.getNumChild() && result == null; i ++) {
            result = lookforGetOperation(await, node.getChild(i), varName);
        }
        return result; 
    }

    private void processFieldDecl (FieldDecl field) {
        System.out.println("Processing Field Declaration: " + field.getName());
        if (field.getChild(0) instanceof DataTypeUse) {
            DataTypeUse type = (DataTypeUse)field.getChild(0);

            dataFieldDecls.put(field,hasFutureVariables(type));
        }
    }

    private boolean hasFutureVariables (ASTNode<?> node) {
        boolean result = false; 
        result = (node instanceof DataTypeUse && 
                "Fut".equals(((DataTypeUse)node).getName())); 

        for (int i = 0; i < node.getNumChild() && !result; i ++) {
            result = result || hasFutureVariables(node.getChild(i));
        }
        return result; 
    }

    public boolean isDeadlockFree () {
        boolean result = true; 
        for (GetExp get: getExpressions.keySet()) {
            result = result && (getExpressions.get(get) != null); 
        }

        for (Boolean value: dataFieldDecls.values()) {
            result = result && !value; 
        }
        return result; 
    }

    public String toString () {
        StringBuffer buffer = new StringBuffer ();
        buffer.append("***** Deadlock Preanalysis Results: *****\n");
        buffer.append("----- GET Expressions evaluation --------\n");
        for(GetExp get: getExpressions.keySet() ) {
            buffer.append(get.toString());
            buffer.append("["); 
            int line = get.getStartLine(); 
            buffer.append(get.getCompilationUnit().getName() + ":" + (line != 0? + line:"_")); 
            buffer.append("]");
            buffer.append(":" + (getExpressions.get(get) != null? true:false)+ "\n");
        }

        buffer.append("----- Field Types -----------------------\n");

        for (FieldDecl field: dataFieldDecls.keySet()) {
            buffer.append(field.getContextDecl().getName() + "." + field.getName() + " [" + 
                    field.getCompilationUnit().getName() + ":" + field.getStartLine() + "]"); 
            buffer.append(" might contain a future value?: " + dataFieldDecls.get(field) + "\n");
        }
        buffer.append("*****************************************");
        return buffer.toString(); 
    }


    private String printTab(int n) {
        StringBuffer buffer = new StringBuffer (); 
        for (int i = 0; i < n; i++) {
            buffer.append("    ");
        }
        return buffer.toString();
    }

}
