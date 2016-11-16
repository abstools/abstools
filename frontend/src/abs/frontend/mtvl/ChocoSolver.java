/*
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */

package abs.frontend.mtvl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import abs.frontend.ast.BoundaryInt;
import abs.frontend.ast.BoundaryVal;
import abs.frontend.ast.Limit;
import abs.frontend.ast.Model;

import org.chocosolver.solver.Solution;
import org.chocosolver.solver.Solver;
import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.exception.ContradictionException;
import org.chocosolver.solver.variables.*;

public class ChocoSolver {
    public final org.chocosolver.solver.Model csmodel;
    public Solver solver;
    public final Map<String, IntVar> varsmap;
    private Constraint[] constraints = new Constraint[1000];
    private IntVar[] vars = new IntVar[1000];
    public int nConstraint, nVars;
    public Model absmodel = new Model();

    public ChocoSolver() {
        csmodel = new org.chocosolver.solver.Model();
        varsmap = new HashMap<String, IntVar>();
        nConstraint = 0;
        nVars = 0;
    }

    public ChocoSolver(Model m) {
        this();
        absmodel = m;
    }

    public void addIntVar(String name, int from, int to) {
        IntVar v = csmodel.intVar(name, from, to);
        vars[nVars] = v;
        nVars++;
        varsmap.put(name, v);
        
        absmodel.println("  adding Int var '" + name + "' (default -> " + from + ")");
    }

    public void addIntVar(String name, int fromto, boolean from) {
        IntVar v = csmodel.intVar(name, fromto);
        
        if (from) addConstraint(csmodel.arithm(v, ">=", fromto));
        else addConstraint(csmodel.arithm(v, "<=", fromto));
        
        absmodel.println("  adding Int var '" + name + "' (default -> " + fromto + ")");
    }

    public void addIntVar(String name, int[] vals) {
        IntVar v = csmodel.intVar(name, vals);
        vars[nVars] = v;
        nVars++;
        
        absmodel.println("  adding Int var '" + name + "' (default -> " + vals[0] + ")");
    }

    public void addIntVar(String name) {
        IntVar v = csmodel.intVar(name, 0);
        vars[nVars] = v;
        nVars++;
        
        absmodel.println("  adding Int var '" + name + "' (default -> 0)");
    }

    public void addBoolVar(String name) {
        IntVar v = null;
        if(nVars == 0) v = csmodel.boolVar(name, true);
        else v = csmodel.boolVar(name);
        vars[nVars] = v;
        nVars++;
        varsmap.put(name, v);
        
        //absmodel.println("adding Bool var '" + name + "'");
    }

    public void addBoundedVar(String name, BoundaryInt b1, BoundaryInt b2) {
        if (b1 instanceof Limit)
            if (b2 instanceof Limit)
                addIntVar(name);
            else
                addIntVar(name,((BoundaryVal) b2).getValue(), false);
        else if (b2 instanceof Limit)
            addIntVar(name,((BoundaryVal) b1).getValue(), true);
        else
            addIntVar(name, ((BoundaryVal) b1).getValue(), ((BoundaryVal) b2).getValue());
    }

    // add constraint
    public void addConstraint(Constraint c) {
        constraints[nConstraint] = c;
        constraints[nConstraint].post();;
        nConstraint++;
    }
    
    // get integer variable
    public IntVar getVar(String name) { return varsmap.get(name); }
    
    // get solution to string
    public String resultToString() {
        Solution solution = getSolution();
        
        StringBuilder result = new StringBuilder();
        IntVar[] ivs = csmodel.retrieveIntVars(true);
        for(int i=0; i<nVars; i++)
        {
            result.append(ivs[i].getName() + " -> " + solution.getIntVal(ivs[i]) + "\n");
        }
              
        return result.toString();
    }
    
    // get solution
    public Solution getSolution() {
        solver = csmodel.getSolver();
        
        return solver.findSolution();
    }
    
    // get solutions to string
    public String resultsToString() {
        List<Solution> solutions = getSolutions();
        
        StringBuilder result = new StringBuilder();
        IntVar[] ivs = csmodel.retrieveIntVars(true);
        int s = 1;
        for(Solution solution : solutions)
        {
            result.append("---Solution " + s + " ---\n");
            for(int i=0; i<nVars; i++)
            {
                result.append(ivs[i].getName() + " -> " + solution.getIntVal(ivs[i]) + "\n");
            }
            s++;
        }
              
        return result.toString();
    }
    
    // get solutions
    public List<Solution> getSolutions() {        
        solver = csmodel.getSolver();
        
        return solver.findAllSolutions();
    }
    
    // count solutions
    public long countSolutions() {               
        solver = csmodel.getSolver();
        solver.findAllSolutions();
        
        return solver.getSolutionCount();
    }
    
    // get minimize to string
    public String minimiseToString(String var) {
        List<Solution> solutions = optimise(var, true);
        
        StringBuilder result = new StringBuilder();
        IntVar[] ivs = csmodel.retrieveIntVars(true);
        int s = 1;
        for(Solution solution : solutions)
        {
            result.append("---Solution " + s + " ---\n");
            for(int i=0; i<nVars; i++)
            {
                result.append(ivs[i].getName() + " -> " + solution.getIntVal(ivs[i]) + "\n");
            }
            s++;
        }
        
        return result.toString();
    }
    
    // get maximize to string
    public String maximiseToString(String var) {
        List<Solution> solutions = optimise(var, false);
        
        StringBuilder result = new StringBuilder();
        IntVar[] ivs = csmodel.retrieveIntVars(true);
        int s = 1;
        for(Solution solution : solutions)
        {
            result.append("---Solution " + s + "---\n");
            for(int i=0; i<nVars; i++)
            {
                result.append(ivs[i].getName() + " -> " + solution.getIntVal(ivs[i]) + "\n");
            }
            s++;
        }
        
        return result.toString();
    }
    
    // get optimized solution
    public List<Solution> optimise(String var, Boolean minimise) {
        solver = csmodel.getSolver();
        List<Solution> solutions;
        if (minimise) solutions = solver.findAllOptimalSolutions(varsmap.get(var), false);
        else solutions = solver.findAllOptimalSolutions(varsmap.get(var), true);
        
        return solutions;
    }
    
    // get maximum product to string
    public String maxProductToString(){
        Solution solution = maxSolution();
        
        StringBuilder result = new StringBuilder();
        IntVar[] ivs = csmodel.retrieveIntVars(true);
        for(int i=0; i<nVars; i++)
        {
            result.append(ivs[i].getName() + " -> " + solution.getIntVal(ivs[i]) + "\n");
        }
              
        return result.toString();
    }
    
    // get maximum solution
    public Solution maxSolution(){
        List<Solution> solutions = getSolutions();
        
        IntVar[] ivs = csmodel.retrieveIntVars(true);
        int maxfeat = 0;
        Solution maxSol = null;
        for(Solution solution : solutions)
        {
            int nfeats = 0;
            for(int i=0; i<nVars; i++)
            {
                if(solution.getIntVal(ivs[i]) == 1) nfeats++;
            }
            if(nfeats>maxfeat){
                maxfeat = nfeats;
                maxSol = solution;
            }  
        }
        
        return maxSol;
    }
    
    // check to string
    public String checkToString(){
        if(countSolutions() == 0) return "checking solution: false";
        else return "checking solution: true"; 
    }
    
    public List<String> checkSolutionWithErrors(Map<String, Integer> solution, Model model) {
        List<String> res = new ArrayList<String>();
        
        // check first for limits of variables
        for (IntVar v : varsmap.values()) {
            org.chocosolver.solver.Model m = new org.chocosolver.solver.Model();
            m.intVar(v.getName(), v.getValue());
            if (!checkSolution(solution, model, m))
                res.add(v.toString());
        }
        
        // now check all explicit constraints
        for(int i=0; i<nConstraint; i++){
            org.chocosolver.solver.Model m = new org.chocosolver.solver.Model();
            m.post(constraints[i]);
        }

        for (Constraint c : constraints) {
            org.chocosolver.solver.Model m = new org.chocosolver.solver.Model();
            m.post(c);
            if (!checkSolution(solution, model, m)) res.add("Error...");
                //res.add(prettyConst(c));
        }
        
        return res;
    }
    
    public boolean checkSolution(Map<String, Integer> solution, Model model, org.chocosolver.solver.Model m) {
        if (absmodel.debug) absmodel.println("solution to check:\n" + solution);
        
        Solver s = m.getSolver();
        IntVar[] ivs = csmodel.retrieveIntVars(true);
        
        try {
            // aux variables
            int val;
            Set<String> newFeatures = new HashSet<String>();
            Set<String> newParents = new HashSet<String>();

            if (absmodel.debug) absmodel.println("Adding new values:");
            
            for(int i=0; i<ivs.length; i++) { // for all variables in the constraints
                // (model): round 1
                // IF used variable is present in the solution, update it!
                if (solution.containsKey(ivs[i].getName())) {
                    val = solution.get(ivs[i].getName());
                    
                    if (absmodel.debug) absmodel.println("  " + ivs[i] + " -> " + val);
                    
                    //s.getVar(ivs[i]).setVal(val);
                    // Possible feature name -- include later the parents.
                    if (val == 1) newFeatures.add(ivs[i].getName());
                }
            }
            // add parents of features from the solution that are not in the
            // constraints (model)
            for (Map.Entry<String, Integer> entry : solution.entrySet()) {
                if (entry.getValue() == 1)
                    if (!entry.getKey().contains("."))
                        newFeatures.add(entry.getKey());
            }

            // collect parents of 'newFeatures'
            if (model != null)
                model.collectParents(newFeatures, newParents);
            // add newParents and default values to the solution
            ivs = csmodel.retrieveIntVars(true);
            for(int i=0; i<ivs.length; i++) { // for all variables in the constraints
                // (model): round 2
                // If it is a parent to include, set
                if (newParents.contains(ivs[i].getName())) {
                    if (absmodel.debug) absmodel.println("  " + ivs[i] + " (parent) -> 1");
                    //s.getVar(var).setVal(1);
                }
                // ELSE use default value
                else if (!solution.containsKey(ivs[i].getName())) {
                    // By default, the optional wrapper "$..." is ALWAYS true
                    if (ivs[i].getName().startsWith("$")) {
                        if (absmodel.debug) absmodel.println("  " + ivs[i] + " (default) -> 1");
                        //s.getVar(var).setVal(1);
                        // By default, unrefered features & attributes are false
                    } /*else if (defaultvals.containsKey(ivs[i].getName())) {
                        int defval = defaultvals.get(ivs[i].getName());
                        if (absmodel.debug) absmodel.println("  " + ivs[i].getName() + " (default) -> " + defval);
                        //s.getVar(var).setVal(defval);
                    } else {
                        if (absmodel.debug) absmodel.println("  " + ivs[i].getName() + " (default) -> 0");
                        //s.getVar(var).setVal(0);
                    }*/
                }
            }
        } 
        /*catch (ContradictionException e1) {
            if (absmodel.debug)
                System.err.println("$$$ Contradiction found... $$$");
        }*/
        // Catch-all
        catch (Exception e1) {
            // Catch-all
            if (absmodel.debug) {
                System.err.println("$$$ Failed to check solution... $$$");
                e1.printStackTrace();
            }
        }
        
        //return csmodel.getSolver().checkSolution();
        return true;
    }
    
    public Set<Set<String>> getSolutionsFeaturesOnly() {
        Set<Set<String>> solutions = new HashSet<Set<String>>();

        return solutions;
    }
}
