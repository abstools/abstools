/*
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */

package abs.frontend.mtvl;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import abs.frontend.ast.BoundaryInt;
import abs.frontend.ast.BoundaryVal;
import abs.frontend.ast.Limit;
import abs.frontend.ast.Model;
import choco.Choco;
import choco.cp.model.CPModel;
import choco.cp.solver.CPSolver;
import choco.kernel.common.logging.ChocoLogging;
import choco.kernel.common.logging.Verbosity;
import choco.kernel.model.constraints.ComponentConstraint;
import choco.kernel.model.constraints.Constraint;
import choco.kernel.model.constraints.ConstraintType;
import choco.kernel.model.constraints.MetaConstraint;
import choco.kernel.model.variables.integer.IntegerExpressionVariable;
import choco.kernel.model.variables.integer.IntegerVariable;
import choco.kernel.solver.ContradictionException;

public class ChocoSolver {

    public final CPModel cpmodel;
    public final CPSolver solver;
    public boolean solved = false;
    public boolean newsol = false;
    public final Map<String, IntegerVariable> vars;
    public final Map<String, Integer> defaultvals;
    // private boolean debug = false;
    private List<Constraint> constraints = new ArrayList<Constraint>();
    private Model absmodel = new Model();

    public ChocoSolver() {
        // Build the model
        cpmodel = new CPModel();
        solver = new CPSolver();
        ChocoLogging.setVerbosity(Verbosity.SILENT);
        vars = new HashMap<String, IntegerVariable>();
        defaultvals = new HashMap<String, Integer>();
        ChocoLogging.setVerbosity(Verbosity.OFF);
    }

    public ChocoSolver(Model m) {
        this();
        if (m.debug)
            ChocoLogging.setVerbosity(Verbosity.DEFAULT);
        absmodel = m;
    }


    /**
     * add int variable
     *
     * @param name
     *            - name of the variable to be added
     * @param from
     *            - lowerlimit of the domain of the variable
     * @param to
     *            - upper limit of the domain of the variable
     **/
    public void addIntVar(String name, int from, int to) {
        IntegerVariable v = Choco.makeIntVar(name, from, to);
        // addConstraint(Choco.geq(v,from)); // needed to include the variable
        // in the constraints to be solved.
        vars.put(name, v);
        defaultvals.put(name, from);
        if (absmodel.debug)
            absmodel.println("  adding Int var '" + name + "' (default -> " + from + ")");
        cpmodel.addVariable(v); // needed to include the variable in the constraints
        // to be solved.
    }

    public void addIntVar(String name, int fromto, boolean from) {
        IntegerVariable v = Choco.makeIntVar(name);
        if (from)
            addConstraint(Choco.geq(v, fromto));
        else
            addConstraint(Choco.leq(v, fromto));
        vars.put(name, v);
        defaultvals.put(name, fromto);
        if (absmodel.debug)
            absmodel.println("  adding Int var '" + name + "' (default -> " + fromto + ")");
        // m.addVariable(v); // not needed, since v is used in the constraints
    }

    public void addIntVar(String name, int[] vals) {
        IntegerVariable v = Choco.makeIntVar(name, vals);
        vars.put(name, v);
        defaultvals.put(name, vals[0]); // vals has at least 1 element! (by the
        // parser constraints)
        if (absmodel.debug)
            absmodel.println("  adding Int var '" + name + "' (default -> " + vals[0] + ")");
        cpmodel.addVariable(v); // needed to include the variable in the constraints
        // to be solved.
    }

    public void addIntVar(String name) {
        IntegerVariable v = Choco.makeIntVar(name);
        vars.put(name, v);
        defaultvals.put(name, 0);
        if (absmodel.debug)
            absmodel.println("  adding Int var '" + name + "' (default -> 0)");
        // m.addVariable(v); // not needed - if variable is not constrained in
        // any way, it should not be considered when solving.
    }

    public void addBoolVar(String name) {
        IntegerVariable v = Choco.makeBooleanVar(name);
        vars.put(name, v);
        defaultvals.put(name, 0);
        if (absmodel.debug)
            absmodel.println("  adding Bool var '" + name + "' (default -> False)");
        // m.addVariable(v); // not needed - if variable is not constrained in
        // any way, it should not be considered when solving.
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

    // Is this method ever called??
    // The mTVL syntax does not seem to allow constraining an attribute to a given set.
    public void addSetVar(String name, BoundaryInt[] bs) {
        int bsize = bs.length - 1;
        int[] vals = new int[bsize];
        // addSetVar only called if bs has only BoundaryVals
        for (int i=0; i < bsize; i++) {
            vals[i] = ((BoundaryVal) bs[i+1]).getValue(); // drop first value - repeated
        }
        addIntVar(name,vals);
    }


    /** set a bool variable to true **/
    public void forceTrue(String name) {
        IntegerVariable v = Choco.makeIntVar(name, 1, 1);
        vars.put(name, v);
        defaultvals.put(name, 1);
        cpmodel.addVariable(v);
    }

    /** add choco constraint **/
    public void addConstraint(Constraint c) {
        constraints.add(c);
        // m.addConstraint(c);
    }

    public IntegerVariable getVar(String var) {
        return vars.get(var);
    }

    public boolean solve() {
        // add the constraints
        if (!solved) {
            for (Constraint c : constraints)
                cpmodel.addConstraint(c);
        }

        // show the problem
        if (absmodel.debug) {
            absmodel.println("## The constraints:");
            // ast.println(m.pretty());
            for (Constraint c : constraints) {
                if (!c.pretty().startsWith("true"))
                    absmodel.println(prettyConst(c));
            }
            absmodel.println("-----");
        }

        // Read the model
        solver.read(cpmodel);
        // Solve the model
        newsol = solver.solve();
        solved = true;
        return newsol;
    }

    public boolean optimise(String var, Boolean minimise) {
        // add the constraints
        if (!solved) {
            for (Constraint c : constraints)
                cpmodel.addConstraint(c);
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
                cpmodel.addConstraint(Choco.eq(vars.get(feat), 1));

                // // collect parents of 'newFeatures'
                // Set<String> newFeatures = new HashSet<String>();
                // Set<String> newParents = new HashSet<String>();
                //
                // if (model != null) {
                // newFeatures.add(feat);
                // model.collectParents(newFeatures,newParents);
                // }
                // // add newParents and default values to the solution
                // Iterator<IntegerVariable> it = m.getIntVarIterator();
                // while (it.hasNext()) { // for all variables in the
                // constraints (model): round 2
                // IntegerVariable var2 = it.next();
                //
                // // If it is a parent to include, set
                // if (newParents.contains(var2.getName())) {
                // if (ast.debug)
                // ast.println("  "+var2+" (parent) -> 1");
                // m.addConstraint(Choco.eq(var2, 1));
                // }
                // }
            }
        }

        // show the problem
        if (absmodel.debug) {
            absmodel.println("## The constraints:");
            // ast.println(m.pretty());
            for (Constraint c : constraints) {
                if (!c.pretty().startsWith("true"))
                    absmodel.println(prettyConst(c));
            }
            absmodel.println("-----");
        }

        // Read the model
        solver.read(cpmodel);
        // Minmise the model, if possible
        if (vars.containsKey(var))
            if (solver.contains(vars.get(var))) {
                solved = true;
                if (minimise)
                    newsol = solver.minimize(solver.getVar(vars.get(var)), true);
                else
                    newsol = solver.maximize(solver.getVar(vars.get(var)), true);
                return newsol;
            }
        return false;
    }

    public int countSolutions() {
        if (absmodel.debug) {
            absmodel.print("## The constraints:");
            absmodel.println(cpmodel.pretty());
        }

        // add the constraints
        for (Constraint c : constraints)
            cpmodel.addConstraint(c);

        // Read the model
        solver.read(cpmodel);
        // Solve the model
        solver.solveAll();

        return solver.getNbSolutions();
    }

    public boolean solveAgain() {
        if (!solved)
            newsol = solve();
        else
            newsol = solver.nextSolution();
        return newsol;
    }

    public Map<String, Integer> getSolution() {
        if (!solved)
            solve();

        HashMap<String, Integer> result = new HashMap<String, Integer>();

        Iterator<IntegerVariable> it = cpmodel.getIntVarIterator();
        while (it.hasNext()) {
            IntegerVariable var = it.next();
            result.put(var.getName(), solver.getVar(var).getVal());
        }
        return result;
    }

    public Set<Map<String,Integer>> getSolutions() {
        Set<Map<String, Integer>> solutions = new HashSet<Map<String,Integer>>();

        while(solveAgain()) {
            Map<String,Integer> sol = new HashMap<String,Integer>();
            Iterator<IntegerVariable> it = cpmodel.getIntVarIterator();
            while (it.hasNext()) {
                IntegerVariable var = it.next();
                sol.put(var.getName(), solver.getVar(var).getVal());
            }
            solutions.add(sol);
        }
        return solutions;
    }

    public Set<Set<String>> getSolutionsFeaturesOnly() {
        Set<Set<String>> solutions = new HashSet<Set<String>>();

        while(solveAgain()) {
            HashSet<String> sol = new HashSet<String>();
            Iterator<IntegerVariable> it = cpmodel.getIntVarIterator();
            while (it.hasNext()) {
                IntegerVariable var = it.next();
                if (solver.getVar(var).getVal() == 1) // We are dealing with features only, where 1 means TRUE
                    sol.add(var.getName());
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

        Iterator<IntegerVariable> it = cpmodel.getIntVarIterator();
        while (it.hasNext()) {
            IntegerVariable var = it.next();
            if (absmodel.debug || !var.getName().startsWith("$"))
                result.append(var.getName() + " -> " + solver.getVar(var).getVal() + "\n");
        }
        return result.toString();
    }

    public int maxValue(String optVar) {
        if (!solved)
            solve();

        if (!newsol)
            return 0;

        Iterator<IntegerVariable> it = cpmodel.getIntVarIterator();
        while (it.hasNext()) {
            IntegerVariable var = it.next();
            if (var.getName().equalsIgnoreCase(optVar)) {
                return solver.getVar(var).getVal();
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

    // public boolean checkSolution(Map<String,Integer> solution, Model model) {
    // boolean res = true;
    // for (Constraint c: constraints) {
    // CPModel m = new CPModel();
    // m.addConstraint(c);
    // boolean csol = checkSolution(solution,model,m);
    // res = res & csol;
    // //if (debug)
    // if (!csol) ast.println("Constraint failed: "+prettyConst(c));
    // }
    //
    // return res;
    // for (Constraint c: constraints)
    // m.addConstraint(c);
    // return checkSolution(solution,model,m);
    // }

    public List<String> checkSolutionWithErrors(Map<String, Integer> solution, Model model) {
        List<String> res = new ArrayList<String>();
        // check first for limits of variables
        for (IntegerVariable v : vars.values()) {
            CPModel m = new CPModel();
            m.addVariable(v);
            if (!checkSolution(solution, model, m))
                res.add(v.toString());
        }
        // now check all explicit constraints
        for (Constraint c : constraints) {
            CPModel m = new CPModel();
            m.addConstraint(c);
            if (!checkSolution(solution, model, m))
                res.add(prettyConst(c));
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
    private static String mbParenthesis(String s) {
        if (s.contains(" "))
            return "(" + s + ")";
        else
            return s;
    }

    private static String prettyConst(Constraint c) {
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
    }

    private static String prettyVar(choco.kernel.model.variables.Variable v) {
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
    }

    public boolean checkSolution(Map<String, Integer> solution, Model model, CPModel m) {
        // Read the model
        CPSolver s = new CPSolver();
        s.read(m);

        if (absmodel.debug)
            absmodel.println("solution to check:\n" + solution);

        // HashMap<String,Integer> selection = new HashMap<String,Integer>();

        Iterator<IntegerVariable> it = m.getIntVarIterator();
        try {
            // aux variables
            int val;
            Set<String> newFeatures = new HashSet<String>();
            Set<String> newParents = new HashSet<String>();

            if (absmodel.debug)
                absmodel.println("Adding new values:");
            while (it.hasNext()) { // for all variables in the constraints
                // (model): round 1
                IntegerVariable var = it.next();
                // IF used variable is present in the solution, update it!
                if (solution.containsKey(var.getName())) {
                    val = solution.get(var.getName());
                    if (absmodel.debug)
                        absmodel.println("  " + var + " -> " + val);
                    s.getVar(var).setVal(val);
                    // Possible feature name -- include later the parents.
                    if (val == 1)
                        newFeatures.add(var.getName());
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
            it = m.getIntVarIterator();
            while (it.hasNext()) { // for all variables in the constraints
                // (model): round 2
                IntegerVariable var = it.next();

                // If it is a parent to include, set
                if (newParents.contains(var.getName())) {
                    if (absmodel.debug)
                        absmodel.println("  " + var + " (parent) -> 1");
                    s.getVar(var).setVal(1);
                }
                // ELSE use default value
                else if (!solution.containsKey(var.getName())) {
                    // By default, the optional wrapper "$..." is ALWAYS true
                    if (var.getName().startsWith("$")) {
                        if (absmodel.debug)
                            absmodel.println("  " + var + " (default) -> 1");
                        s.getVar(var).setVal(1);
                        // By default, unrefered features & attributes are false
                    } else if (defaultvals.containsKey(var.getName())) {
                        int defval = defaultvals.get(var.getName());
                        if (absmodel.debug)
                            absmodel.println("  " + var.getName() + " (default) -> " + defval);
                        s.getVar(var).setVal(defval);
                    } else {
                        if (absmodel.debug)
                            absmodel.println("  " + var.getName() + " (default) -> 0");
                        s.getVar(var).setVal(0);
                    }
                }
            }
        } catch (ContradictionException e1) {
            if (absmodel.debug)
                System.err.println("$$$ Contradiction found... $$$");
        }
        // Catch-all
        catch (Exception e1) {
            // Catch-all
            if (absmodel.debug) {
                System.err.println("$$$ Failed to check solution... $$$");
                e1.printStackTrace();
            }
        }

        // if (debug) {
        // String result = "";
        // it = m.getIntVarIterator();
        // while (it.hasNext()) {
        // IntegerVariable var = (IntegerVariable) it.next();
        // result = result + var.getName() + " -> "+s.getVar(var).getVal() +
        // "\n";
        // }
        // output.println("Trying:\n"+result);
        // }

        return s.checkSolution();
    }

    public static Constraint eqeq(IntegerVariable v1, IntegerVariable v2) {
        return Choco.eq(v1, v2);
    }

    public static Constraint eqeq(IntegerExpressionVariable v1, IntegerExpressionVariable v2) {
        return Choco.eq(v1, v2);
    }

    public static Constraint eqeq(IntegerExpressionVariable v1, int v2) {
        return Choco.eq(v1, v2);
    }

    public static Constraint isTrue(IntegerExpressionVariable v1) {
        return Choco.eq(v1, 1);
    }

    public CPSolver getCPSolver() {
        return solver;
    }
}
