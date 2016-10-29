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

import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.variables.*;

public class ChocoSolver {
    public final org.chocosolver.solver.Model model;
    public boolean solved = false;
    public boolean newsol = false;
    public final Map<String, BoolVar> vars;
    private Constraint[] constraints;
    private Model absmodel = new Model();

    public ChocoSolver() {
        model = new org.chocosolver.solver.Model();
        vars = new HashMap<String, BoolVar>();
    }

    public ChocoSolver(Model m) {
        this();
        absmodel = m;
    }

    public void addIntVar(String name, int from, int to) {
        IntVar v = model.intVar(name, from, to);
        //vars.put(name, v);
    }

    public void addIntVar(String name, int fromto, boolean from) {
        IntVar v = model.intVar(name, fromto);
        //vars.put(name, v);
    }

    public void addIntVar(String name, int[] vals) {
        IntVar v = model.intVar(name, vals);
        //vars.put(name, v);
    }

    public void addIntVar(String name) {
        IntVar v = model.intVar(name, 0);
        //vars.put(name, v);
    }

    public void addBoolVar(String name) {
        BoolVar v = model.boolVar(name);
        vars.put(name, v);
    }

    /*public void addBoundedVar(String name, BoundaryInt b1, BoundaryInt b2) {
        if (b1 instanceof Limit)
            if (b2 instanceof Limit)
                addIntVar(name);
            else
                addIntVar(name,((BoundaryVal) b2).getValue(), false);
        else if (b2 instanceof Limit)
            addIntVar(name,((BoundaryVal) b1).getValue(), true);
        else
            addIntVar(name, ((BoundaryVal) b1).getValue(), ((BoundaryVal) b2).getValue());
    }*/

    public void forceTrue(String name) {
        BoolVar v = model.boolVar(name, true);
        vars.put(name, v);
    }

    public void addConstraint(Constraint c) {
        constraints[constraints.length] = c;
    }

    public BoolVar getVar(String var) {
        return vars.get(var);
    }

    public boolean solve() {
        if (!solved) {
            model.post(constraints);
        }

        newsol = model.getSolver().solve();
        solved = true;
        
        return newsol;
    }

    public long countSolutions() {       
        model.post(constraints);
        return model.getSolver().getSolutionCount();
    }

    public boolean solveAgain() {
        if (!solved)
            newsol = solve();
        else
            newsol = model.getSolver().solve();
        
        return newsol;
    }
    
    public String resultToString() {
        printVars();
        
        if (!solved)
            solve();

        if (!newsol)
            return "-- No (more) solutions --\n";

        StringBuilder result = new StringBuilder();
        IntVar[] ivs = model.retrieveIntVars(false);
        
        for(int i=0; i<ivs.length;i++)
        {
            result.append(ivs[i].getName() + " -> " + ivs[i].getValue() + "\n");
        }
              
        return result.toString();
    }
    
    public List<String> checkSolutionWithErrors(Map<String, Integer> solution, Model model) {
        List<String> res = new ArrayList<String>();
        // check first for limits of variables
        /*for (IntVar v : vars.values()) {
            org.chocosolver.solver.Model m = new org.chocosolver.solver.Model();
            m.intVar(, v);
            if (!checkSolution(solution, model, m))
                res.add(v.toString());
        }
        // now check all explicit constraints
        for (Constraint c : constraints) {
            Model m = new Model();
            m.addConstraint(c);
            if (!checkSolution(solution, model, m))
                res.add(prettyConst(c));
        }*/
        
        return res;
    }
    
    public Set<Set<String>> getSolutionsFeaturesOnly() {
        Set<Set<String>> solutions = new HashSet<Set<String>>();

        /*while(solveAgain()) {
            HashSet<String> sol = new HashSet<String>();
            Iterator<IntegerVariable> it = cpmodel.getIntVarIterator();
            while (it.hasNext()) {
                IntegerVariable var = it.next();
                if (solver.getVar(var).getVal() == 1) // We are dealing with features only, where 1 means TRUE
                    sol.add(var.getName());
            }
            solutions.add(sol);
        }*/
        
        return solutions;
    }
    
    void printVars()
    {
        System.out.println(vars.size());
    }
}
