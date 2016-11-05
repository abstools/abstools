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
import org.chocosolver.solver.expression.discrete.arithmetic.ArExpression;
import org.chocosolver.solver.variables.*;

public class ChocoSolver {
    public final org.chocosolver.solver.Model csmodel;
    public boolean solved = false;
    public boolean newsol = false;
    public final Map<String, IntVar> vars;
    public final Map<String, Integer> defaultvals;
    private Constraint[] constraints;
    private Model absmodel = new Model();

    public ChocoSolver() {
        csmodel = new org.chocosolver.solver.Model();
        vars = new HashMap<String, IntVar>();
        defaultvals = new HashMap<String, Integer>();
    }

    public ChocoSolver(Model m) {
        this();
        absmodel = m;
    }

    public void addIntVar(String name, int from, int to) {
        IntVar v = csmodel.intVar(name, from, to);
        defaultvals.put(name, from);
        vars.put(name, v);
        
        if (absmodel.debug)
            absmodel.println("  adding Int var '" + name + "' (default -> " + from + ")");
    }

    public void addIntVar(String name, int fromto, boolean from) {
        IntVar v = csmodel.intVar(name, fromto);
        
        if (from)
            addConstraint(csmodel.arithm(v, ">=", fromto));
        else
            addConstraint(csmodel.arithm(v, "<=", fromto));
        
        vars.put(name, v);
        defaultvals.put(name, fromto);
        
        if (absmodel.debug)
            absmodel.println("  adding Int var '" + name + "' (default -> " + fromto + ")");
    }

    public void addIntVar(String name, int[] vals) {
        IntVar v = csmodel.intVar(name, vals);
        vars.put(name, v);
        defaultvals.put(name, vals[0]);
        
        if (absmodel.debug)
            absmodel.println("  adding Int var '" + name + "' (default -> " + vals[0] + ")");
    }

    public void addIntVar(String name) {
        IntVar v = csmodel.intVar(name, 0);
        vars.put(name, v);
        
        defaultvals.put(name, 0);
        if (absmodel.debug)
            absmodel.println("  adding Int var '" + name + "' (default -> 0)");
    }

    public void addBoolVar(String name) {
        IntVar v = csmodel.boolVar(name);
        vars.put(name, v);
        defaultvals.put(name, 0);
        
        if (absmodel.debug)
            absmodel.println("  adding Bool var '" + name + "' (default -> False)");
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

    public void forceTrue(String name) {
        BoolVar v = csmodel.boolVar(name, true);
        vars.put(name, v);
        defaultvals.put(name, 1);
    }

    public void addConstraint(Constraint c) {
        constraints[constraints.length] = c;
    }

    public IntVar getVar(String var) {
        return vars.get(var);
    }

    public boolean solve() {
        if (!solved) {
            csmodel.post(constraints);
        }

        newsol = csmodel.getSolver().solve();
        solved = true;
        
        return newsol;
    }
    
    public boolean optimise(String var, Boolean minimise) {
        // add the constraints
        if (!solved) {
            for (Constraint c : constraints)
                csmodel.post(c);
        }

        // Impose the feature name to be present
        if (var.contains(".")) {
            String feat = var.split("\\.")[0];
            if (absmodel.debug)
                absmodel.println("## including feature '" + feat + "'"); // and its
            // parents.");
            if (vars.containsKey(feat)) {
                if (absmodel.debug)
                    absmodel.println("  " + feat + " (selected) -> 1");
                csmodel.post(csmodel.arithm(vars.get(feat), "=", 1));
            }
        }

        // show the problem
        if (absmodel.debug) {
            absmodel.println("## The constraints:");
            // ast.println(m.pretty());
            /*for (Constraint c : constraints) {
                if (!c.pretty().startsWith("true"))
                    absmodel.println(prettyConst(c));
            }*/
            absmodel.println("-----");
        }

        // Minmise the model, if possible
        /*if (vars.containsKey(var))
            if (csmodel.getSolver().contains(vars.get(var))) {
                solved = true;
                if (minimise)
                    newsol = solver.minimize(solver.getVar(vars.get(var)), true);
                else
                    newsol = solver.maximize(solver.getVar(vars.get(var)), true);
                return newsol;
            }*/
        
        return false;
    }
    
    public long countSolutions() {       
        csmodel.post(constraints);
        
        return csmodel.getSolver().getSolutionCount();
    }

    public boolean solveAgain() {
        if (!solved)
            newsol = solve();
        else
            newsol = csmodel.getSolver().solve();
        
        return newsol;
    }
    
    public Map<String, Integer> getSolution() {
        if (!solved)
            solve();

        HashMap<String, Integer> result = new HashMap<String, Integer>();
        IntVar[] ivs = csmodel.retrieveIntVars(false);
        
        for(int i=0; i<ivs.length; i++)
        {
            result.put(ivs[i].getName(), ivs[i].getValue());
        }
        
        return result;
    }
    
    public Set<Map<String,Integer>> getSolutions() {
        Set<Map<String, Integer>> solutions = new HashSet<Map<String,Integer>>();

        while(solveAgain()) {
            Map<String,Integer> sol = new HashMap<String,Integer>();
            IntVar[] ivs = csmodel.retrieveIntVars(false);
            for(int i=0; i<ivs.length; i++)
            {
                sol.put(ivs[i].getName(), ivs[i].getValue());
            }
            solutions.add(sol);
        }
        
        return solutions;
    }
    
    public Set<Set<String>> getSolutionsFeaturesOnly() {
        Set<Set<String>> solutions = new HashSet<Set<String>>();

        while(solveAgain()) {
            HashSet<String> sol = new HashSet<String>();
            IntVar[] ivs = csmodel.retrieveIntVars(false);
            for(int i=0; i<ivs.length; i++)
            {
                if (ivs[i].getValue() == 1)
                    sol.add(ivs[i].getName());
            }
            solutions.add(sol);
        }
        
        return solutions;
    }
    
    public String resultToString() {
        if (!solved)
            solve();

        if (!newsol)
            return "-- No (more) solutions --\n";

        StringBuilder result = new StringBuilder();
        IntVar[] ivs = csmodel.retrieveIntVars(false);
        
        for(int i=0; i<ivs.length; i++)
        {
            result.append(ivs[i].getName() + " -> " + ivs[i].getValue() + "\n");
        }
              
        return result.toString();
    }
    
    public int maxValue(String optVar) {
        if (!solved)
            solve();

        if (!newsol)
            return 0;

        IntVar[] ivs = csmodel.retrieveIntVars(false);
        
        for(int i=0; i<ivs.length; i++)
        {
            if (ivs[i].getName().equalsIgnoreCase(optVar)) {
                return ivs[i].getValue();
            }
        }
        
        return 0;
    }
    
    public String minimiseToString(String var) {
        optimise(var, true);
        return resultToString();
    }
    
    public String maximiseToString(String var) {
        if (absmodel.debug)
            absmodel.println("optimising " + var);
        optimise(var, false);
        return resultToString();
    }
    
    public int maximiseToInt(String var) {
        if (absmodel.debug)
            absmodel.println("optimising " + var);
        optimise(var, false);
        
        return maxValue(var);
    }
    
    public boolean checkSolution(Map<String, Integer> solution) {
        return checkSolution(solution, null);
    }
    
    public List<String> checkSolutionWithErrors(Map<String, Integer> solution, Model model) {
        List<String> res = new ArrayList<String>();
        // check first for limits of variables
        for (IntVar v : vars.values()) {
            org.chocosolver.solver.Model m = new org.chocosolver.solver.Model();
            m.intVar(v.getName(), v.getValue());
            if (!checkSolution(solution, model, m))
                res.add(v.toString());
        }
        // now check all explicit constraints
        for (Constraint c : constraints) {
            org.chocosolver.solver.Model m = new org.chocosolver.solver.Model();
            m.post(c);
            //if (!checkSolution(solution, model, m))
                //res.add(prettyConst(c));
        }
        
        return res;
    }
    
    public boolean checkSolution(Map<String, Integer> solution, Model model) {
        List<String> errors = checkSolutionWithErrors(solution, model);
        
        for (String s : errors)
            absmodel.println("Constraint failed: " + s);
        
        return errors.isEmpty();
    }
    
    // Adds parenthesis for readability if there are *spaces* in the string.
    /*private static String mbParenthesis(String s) {
        if (s.contains(" "))
            return "(" + s + ")";
        else
            return s;
    }*/
    
    /*private static String prettyConst(Constraint c) {
        // System.out.println( "["+c.getClass()+"] "+c.pretty());
        if (c instanceof MetaConstraint) {
            MetaConstraint<?> mc = (MetaConstraint<?>) c;
            if (mc.getConstraintType() == ConstraintType.IMPLIES)
                return mbParenthesis(prettyConst(mc.getConstraint(0))) + " -> "
                + mbParenthesis(prettyConst(mc.getConstraint(1)));
            if (mc.getConstraintType() == ConstraintType.AND)
                return mbParenthesis(prettyConst(mc.getConstraint(0))) + " /\\ "
                + mbParenthesis(prettyConst(mc.getConstraint(1)));
            if (mc.getConstraintType() == ConstraintType.OR)
                return mbParenthesis(prettyConst(mc.getConstraint(0))) + " \\/ "
                + mbParenthesis(prettyConst(mc.getConstraint(1)));
            // output.println("I'm a imply!\nleft: "+mc.getConstraint(0).pretty());
        }
        if (c instanceof ComponentConstraint) {
            ComponentConstraint cc = (ComponentConstraint) c;
            // return cc.getVariable(0) + "[[]]";
            if (c.getConstraintType() == ConstraintType.EQ)
                // if (c.getConstraintType().getName().equals("eq")
                if (c.pretty().endsWith("[0, 1], 1 } )"))
                    // cc.getVariable(0).getConstraint(1).pretty()=="1" &&
                    // mc.getConstraint(0).pretty().endsWith("[0, 1]"))
                    return cc.getVariable(0).getName();
                else if (c.pretty().endsWith("[1, 1], 1 } )"))
                    // cc.getVariable(0).getConstraint(1).pretty()=="1" &&
                    // mc.getConstraint(0).pretty().endsWith("[0, 1]"))
                    return cc.getVariable(0).getName() + "[true]";
                else
                    return prettyVar(cc.getVariable(0)) + " = " + prettyVar(cc.getVariable(1));
            if (c.getConstraintType() == ConstraintType.GEQ)
                return prettyVar(cc.getVariable(0)) + " >= " + prettyVar(cc.getVariable(1));
            if (c.getConstraintType() == ConstraintType.LEQ)
                return prettyVar(cc.getVariable(0)) + " <= " + prettyVar(cc.getVariable(1));
            if (c.getConstraintType() == ConstraintType.GT)
                return prettyVar(cc.getVariable(0)) + " > " + prettyVar(cc.getVariable(1));
            if (c.getConstraintType() == ConstraintType.LT)
                return prettyVar(cc.getVariable(0)) + " < " + prettyVar(cc.getVariable(1));
        }
        return // "["+c.getClass()+"] "+c.pretty();
                c.pretty();
    }*/
    
    /*private static String prettyVar(choco.kernel.model.variables.Variable v) {
        // System.out.println( "--["+v+": "+v.getClass()+"] "+v.pretty());
        if (v instanceof IntegerExpressionVariable) {
            IntegerExpressionVariable exp = (IntegerExpressionVariable) v;

            if (exp.getOperator().name() == "SUM") {
                String res = "";
                if (exp.getNbVars() > 0) {
                    res += prettyVar(exp.getVariable(0));
                    for (int i = 1; i < exp.getNbVars(); i++)
                        res += " + " + prettyVar(exp.getVariable(i));
                } else
                    res = "0";
                return res;
            }
        }
        if (v instanceof IntegerVariable) {
            IntegerVariable iv = (IntegerVariable) v;
            if (iv.isBoolean())
                return iv.getName();
        }
        return v.pretty();
    }*/
    
    public boolean checkSolution(Map<String, Integer> solution, Model model, org.chocosolver.solver.Model m) {
        if (absmodel.debug)
            absmodel.println("solution to check:\n" + solution);

        // HashMap<String,Integer> selection = new HashMap<String,Integer>();

        IntVar[] ivs = csmodel.retrieveIntVars(false);
        
        try {
            // aux variables
            int val;
            Set<String> newFeatures = new HashSet<String>();
            Set<String> newParents = new HashSet<String>();

            if (absmodel.debug)
                absmodel.println("Adding new values:");
            
            for(int i=0; i<ivs.length; i++) { // for all variables in the constraints
                // (model): round 1
                // IF used variable is present in the solution, update it!
                if (solution.containsKey(ivs[i].getName())) {
                    val = solution.get(ivs[i].getName());
                    
                    if (absmodel.debug)
                        absmodel.println("  " + ivs[i] + " -> " + val);
                    
                    //s.getVar(var).setVal(val);
                    // Possible feature name -- include later the parents.
                    if (val == 1)
                        newFeatures.add(ivs[i].getName());
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
            ivs = csmodel.retrieveIntVars(false);
            for(int i=0; i<ivs.length; i++) { // for all variables in the constraints
                // (model): round 2
                // If it is a parent to include, set
                if (newParents.contains(ivs[i].getName())) {
                    if (absmodel.debug)
                        absmodel.println("  " + ivs[i] + " (parent) -> 1");
                    //s.getVar(var).setVal(1);
                }
                // ELSE use default value
                else if (!solution.containsKey(ivs[i].getName())) {
                    // By default, the optional wrapper "$..." is ALWAYS true
                    if (ivs[i].getName().startsWith("$")) {
                        if (absmodel.debug)
                            absmodel.println("  " + ivs[i] + " (default) -> 1");
                        //s.getVar(var).setVal(1);
                        // By default, unrefered features & attributes are false
                    } else if (defaultvals.containsKey(ivs[i].getName())) {
                        int defval = defaultvals.get(ivs[i].getName());
                        if (absmodel.debug)
                            absmodel.println("  " + ivs[i].getName() + " (default) -> " + defval);
                        //s.getVar(var).setVal(defval);
                    } else {
                        if (absmodel.debug)
                            absmodel.println("  " + ivs[i].getName() + " (default) -> 0");
                        //s.getVar(var).setVal(0);
                    }
                }
            }
        } 
        //catch () {
        //    if (absmodel.debug)
        //        System.err.println("$$$ Contradiction found... $$$");
        //}
        // Catch-all
        catch (Exception e1) {
            // Catch-all
            if (absmodel.debug) {
                System.err.println("$$$ Failed to check solution... $$$");
                e1.printStackTrace();
            }
        }
        
        //return csmodel.getSolver()..checkSolution();
        return true;
    }
    
    /*public static Constraint eqeq(IntVar v1, IntVar v2) {
        return csmodel.arithm(v1, "=", v2);
    }*/

    /*public static Constraint eqeq(IntegerExpressionVariable v1, IntegerExpressionVariable v2) {
        return csmodel.arithm(v1, "=", v2);
    }*/

    /*public static Constraint eqeq(IntegerExpressionVariable v1, int v2) {
        return csmodel.arithm(v1, "=", v2);
    }*/

    /*public static Constraint isTrue(IntegerExpressionVariable v1) {
        return csmodel.arithm(v1, "=", v2);
    }*/

    /*public CPSolver getCPSolver() {
        return solver;
    }*/
}
