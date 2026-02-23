/*
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */

package org.abs_models.frontend.mtvl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.abs_models.frontend.analyser.SemanticConditionList;
import org.abs_models.frontend.ast.*;
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

    private final CPModel cpmodel = new CPModel();
    private final CPSolver solver = new CPSolver();
    private boolean solved = false;
    private boolean newsol = false;
    private final Map<String, IntegerVariable> vars = new HashMap<>();
    private final Map<String, Integer> defaultvals = new HashMap<>();
    private List<Constraint> constraints = new ArrayList<>();
    private final Model absmodel;

    public static ChocoSolver fromModel(Model m) {
        ChocoSolver solver = new ChocoSolver(m);

        // new int variable for all int variables
        for (java.util.Map.Entry<String, BoundaryInt[]> entry : m.ints().entrySet()) {
            String st = entry.getKey();
            if (entry.getValue().length == 2) {
                BoundaryInt b1 = entry.getValue()[0];
                BoundaryInt b2 = entry.getValue()[1];
                solver.addBoundedVar(st, b1, b2);
            }
            else {
                solver.addSetVar(st, entry.getValue());
            }
        }
        for (String st : m.bools())
            solver.addBoolVar(st);
        for (String st : m.features())
            solver.addBoolVar(st);

        solver.collectConstraints(m); // is adding intvars to the model!
        return solver;
    }

    private ChocoSolver(Model m) {
        absmodel = m;
        if (m.debug)
            ChocoLogging.setVerbosity(Verbosity.DEFAULT);
        else
            ChocoLogging.setVerbosity(Verbosity.OFF);
    }

    /** The variables added to the model.
     */
    private Map<String, IntegerVariable> getVars() {
        return vars;
    };


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

    private void addBoolVar(String name) {
        IntegerVariable v = Choco.makeBooleanVar(name);
        vars.put(name, v);
        defaultvals.put(name, 0);
        if (absmodel.debug)
            absmodel.println("  adding Bool var '" + name + "' (default -> False)");
        // cpmodel.addVariable(v); // not needed - if variable is not constrained in
        // any way, it should not be considered when solving.
    }

    public void addBoundedVar(String name, BoundaryInt b1, BoundaryInt b2) {
        if (b1 instanceof Limit)
            if (b2 instanceof Limit) {
                IntegerVariable v = Choco.makeIntVar(name);
                vars.put(name, v);
                defaultvals.put(name, 0);
                if (absmodel.debug)
                    absmodel.println("  adding Int var '" + name + "' (default -> 0)");
                // cpmodel.addVariable(v); // not needed - if variable is not constrained in
                // any way, it should not be considered when solving.
            } else {
                IntegerVariable v = Choco.makeIntVar(name);
                int b = ((BoundaryVal) b2).getValue();
                addConstraint(Choco.leq(v, b));
                vars.put(name, v);
                defaultvals.put(name, b);
                if (absmodel.debug)
                    absmodel.println("  adding Int var '" + name + "' (default -> " + b + ")");
                // cpmodel.addVariable(v); // not needed, since v is used in the constraints
            }
        else if (b2 instanceof Limit) {
            IntegerVariable v = Choco.makeIntVar(name);
            int b = ((BoundaryVal) b2).getValue();
            addConstraint(Choco.geq(v, b));
            vars.put(name, v);
            defaultvals.put(name, b);
            if (absmodel.debug)
                absmodel.println("  adding Int var '" + name + "' (default -> " + b + ")");
            // cpmodel.addVariable(v); // not needed, since v is used in the constraints
            }
        else
            addIntVar(name, ((BoundaryVal) b1).getValue(), ((BoundaryVal) b2).getValue());
    }

    private void addSetVar(String name, BoundaryInt[] bs) {
        int bsize = bs.length - 1;
        int[] vals = new int[bsize];
        // addSetVar only called if bs has only BoundaryVals
        for (int i=0; i < bsize; i++) {
            vals[i] = ((BoundaryVal) bs[i+1]).getValue(); // drop first value - repeated
        }
        IntegerVariable v = Choco.makeIntVar(name, vals);
        vars.put(name, v);
        defaultvals.put(name, vals[0]); // vals has at least 1 element! (by the
        // parser constraints)
        if (absmodel.debug)
            absmodel.println("  adding Int var '" + name + "' (default -> " + vals[0] + ")");
        cpmodel.addVariable(v); // needed to include the variable in the constraints to be solved.
    }

    /** set a bool variable to true **/
    private void forceTrue(String name) {
        IntegerVariable v = Choco.makeIntVar(name, 1, 1);
        vars.put(name, v);
        defaultvals.put(name, 1);
        cpmodel.addVariable(v);
    }

    /**
     * Add all feature constraints of a product.  Note that the
     * features must have been added already.
     */
    public void addProductConstraints(Product m) {
        HashSet<Constraint> newcs = new HashSet<>();
        for (Feature f: m.getFeatures()) {
            addConstraint(isTrue(vars.get(f.getName())));
            for (AttrAssignment aa: f.getAttrAssignments()) {
                String fname = f.getName() + "." + aa.getName();
                if (vars.containsKey(fname))
                    addConstraint(Choco.eq(vars.get(fname), aa.getValue().getIntValue().intValue()));
            }
        }
    }

    /**
     * Add constraints to solver, or construct them from
     * subconstraints.  The return value is ignorable in some branches
     * since we call addConstraint where appropriate and return
     * Choco.TRUE.  In other branches, we assemble constraints from
     * non-trivial constraint subexpressions so the return value is
     * significant.
     */
    public Constraint collectConstraints(ASTNode s) {
        switch (s) {
            // ROOT: has to be present
            case CompilationUnit c: {
                for (int i = 0; i < c.getNumFeatureDecl(); i++) {
                    forceTrue(c.getFeatureDecl(i).getName());
                }
                for(int i = 0; i < c.getNumChild(); i++)
                    collectConstraints(c.getChildNoTransform(i));
                return Choco.TRUE;
            }
            // FNODE
            case FNode f:
                return collectConstraints(f.getFeatureDecl());
            // FEATURE -> collect constraints, check cardinality, and check children.
            case FeatureDecl f: {
                AttrConstraints acl = f.getAttrConstraints();
                for(int i = 0; i < acl.getNumConstr(); i++)
                    addConstraint(collectConstraints(acl.getConstr(i)));
                if (f.hasGroup())
                    includeGroupConstraints(f.getGroup(),f.getName());
                return Choco.TRUE;
            }
            // FExt
            case FExt f: {
                AttrConstraints acl = f.getAttrConstraints();
                for(int i = 0; i < acl.getNumConstr(); i++)
                    addConstraint(collectConstraints(acl.getConstr(i)));
                if (f.hasGroup())
                    includeGroupConstraints(f.getGroup(),f.getName());
                return Choco.TRUE;
            }
            // IFIN/IFOUT
            case IfIn i:
                return Choco.implies(isTrue(getVar(i.pname())), collectConstraints(i.getExpr()));
            case IfOut i:
                return Choco.implies(Choco.not(isTrue(getVar(i.pname()))), collectConstraints(i.getExpr()));
            // REQUIRE
            case Require r:
                return Choco.implies(isTrue(getVar(r.pname())), isTrue(getVar(r.getFeatVar().getFName())));
            // EXCLUDE
            case Exclude e:
                return Choco.nand(getVar(e.pname()), getVar(e.getFeatVar().getFName()));
            // EXPRESSIONS
            // EXP:VARS
            case Variable v:
                return isTrue(getVar(v.getFullName()));
            //EXP:VALUES
            // > ????
            case MValue m:
                return collectConstraints(m.getValue());
            // < ?????
            case BoolVal b:
                if (b.getValue())
                    return Choco.TRUE;
                else
                    return Choco.FALSE;
            // EXP: Unary
            case MNegExp m:
                return Choco.not(collectConstraints(m.getOperand()));
            // EXP: EqualityExpr
            case MEqExp m:
                // Need to type check JUST the equality of
                // expressions, to know if the constraints should
                // produce '==' or '<->'.  All other type checking
                // should be done after flattening.
                m.checkType(Types.BOOL, new SemanticConditionList());
                if (m.isInt)
                    return Choco.eq(collectIntExpr(m.getLeft()),collectIntExpr(m.getRight()));
                else            // boolean
                    return Choco.ifOnlyIf(collectConstraints(m.getLeft()),collectConstraints(m.getRight()));
            // EXP: RelationalExpr
            // >=
            case MGTEQExp m:
                return Choco.geq(collectIntExpr(m.getLeft()),collectIntExpr(m.getRight()));
            // <=
            case MLTEQExp m:
                return Choco.leq(collectIntExpr(m.getLeft()),collectIntExpr(m.getRight()));
            // >
            case MGTExp m:
                return Choco.gt(collectIntExpr(m.getLeft()),collectIntExpr(m.getRight()));
            // <
            case MLTExp m:
                return Choco.lt(collectIntExpr(m.getLeft()),collectIntExpr(m.getRight()));
            // EXP: BoolExp
            // <=>
            case  MEquivExp m:
                return Choco.ifOnlyIf(collectConstraints(m.getLeft()),collectConstraints(m.getRight()));
            // =>
            case MImpliesExp m:
                return Choco.implies(collectConstraints(m.getLeft()),collectConstraints(m.getRight()));
            // \/
            case  MOrBoolExp m:
                return Choco.or(collectConstraints(m.getLeft()),collectConstraints(m.getRight()));
                // /\
            case MAndBoolExp m:
                return Choco.and(collectConstraints(m.getLeft()),collectConstraints(m.getRight()));
            // EXP: GENERAL (always overwritten when the program type-checks)
            case MExp m:
                return Choco.TRUE;
            case Value v:
                // null is overwritten always for well-typed elements
                return Choco.TRUE;
            // GENERAL NODE: propagate
            case ASTNode s1: {
                for(int i = 0; i < s1.getNumChild(); i++)
                    collectConstraints(s.getChildNoTransform(i));
                return Choco.TRUE;
            }
        }
    }

    private IntegerExpressionVariable collectIntExpr(ASTNode mexp) {
        // argument can be MExp or Value
        switch (mexp) {
            // EXPRESSIONS
            // EXP:VARS
            case Variable v:
                return getVar(v.getFullName());
            //EXP:VALUES
            // > ????
            case MValue m:
                return collectIntExpr(m.getValue());
            // < ?????
            case IntVal v:
                return Choco.constant(v.getValue());
            // EXP: AddExp
            case MAddAddExp m:
                return Choco.plus(collectIntExpr(m.getLeft()),collectIntExpr(m.getRight()));
            case MSubAddExp m:
                return Choco.minus(collectIntExpr(m.getLeft()),collectIntExpr(m.getRight()));
            // EXP: MultExp
            case  MMultMultExp m:
                return Choco.mult(collectIntExpr(m.getLeft()),collectIntExpr(m.getRight()));
            case  MDivMultExp m:
                return Choco.div(collectIntExpr(m.getLeft()),collectIntExpr(m.getRight()));
            case  MModMultExp m:
                return Choco.mod(collectIntExpr(m.getLeft()),collectIntExpr(m.getRight()));
                // EXP: Unary
            case  MMinusExp m:
                return Choco.neg(collectIntExpr(m.getOperand()));
            // EXP: GENERAL (always overwritten when the program type-checks)
            case MExp m:
                return null;
            case Value v:
                // null is overwritten always for well-typed elements
                return null;
            default:
                return null;
        }
    }

    public void includeGroupConstraints(Group g, String varName) {
        IntegerVariable fvar = getVar(varName);
        int nfeats = g.getNumFNode();
        String fname = "";
        IntegerVariable[] feats = new IntegerVariable[nfeats];
        for (int i = 0; i < nfeats; i++) {
            fname = g.getFNode(i).getFeatureDecl().getName();
            IntegerVariable v = null;
            // add intermediate variable $f if f is optional.
            if (g.getFNode(i) instanceof OptFeat) {
                v = Choco.makeBooleanVar("$"+fname);
                // f -> $f
                addConstraint(
                    Choco.implies(isTrue(getVar(fname)),isTrue(v)));
            } else {
                v = getVar(fname);
            }
            // f -> fparent
            addConstraint(Choco.implies(isTrue(v),isTrue(fvar)));
            // rec - FNode
            addConstraint(collectConstraints(g.getFNode(i)));
            feats[i] = v;
            // f -> $f /\ f -> fparent /\ [f]
        }
        // n1 <= $f1 + ... + $fn <= n2
        if (g.getCard() instanceof AllOf)
            // f ->  #feats = nfeats
            addConstraint(Choco.implies(isTrue(fvar),
                Choco.eq(Choco.sum(feats),nfeats)));
        else if (g.getCard() instanceof Minim)
            // f ->  #feats >= from
            addConstraint(Choco.implies(
                isTrue(fvar),
                Choco.geq(Choco.sum(feats),((Minim) g.getCard()).getCFrom())));
        else {
            // f ->  to >= #feats >= from
            addConstraint(Choco.implies(
                isTrue(fvar),
                Choco.geq(Choco.sum(feats),((CRange) g.getCard()).getCFrom())));
            addConstraint(Choco.implies(
                isTrue(fvar),
                Choco.leq(Choco.sum(feats),((CRange) g.getCard()).getCTo())));
        }
    }

    public void addMaxConstraint(Model m, String maxVar) {
        HashSet<Constraint> newcs = new HashSet<>();
        Map<String, IntegerVariable> vars = getVars();
        IntegerExpressionVariable v = Choco.ZERO;
        for(String fname: m.features()){
            if (vars.containsKey(fname))
                v = Choco.plus(v, vars.get(fname));
        }
        addConstraint(Choco.eq(vars.get(maxVar),v));
    }

    public void addDiffConstraint(Model m, Product p, String diffVar) {
        Map<String,IntegerVariable> vars = getVars();
        //calculating deselected features, initially initialized by all features
        ArrayList<String> deselectedFeatures = new ArrayList();
        for (String fname: m.features()) {
            deselectedFeatures.add(fname);
        }

        //removing the selected features to get deselected features
        //
        IntegerExpressionVariable v = Choco.ZERO;
        for (Feature f: p.getFeatures()) {
            v = Choco.plus(v, Choco.abs(Choco.minus(vars.get(f.getName()), 1)));
            for (String fname: deselectedFeatures) {
                if(f.getName().equalsIgnoreCase(fname)) {
                    deselectedFeatures.remove(fname);
                    break;
                }
            }
        }

        for(String fname: deselectedFeatures){
            v = Choco.plus(v, vars.get(fname));
        }
        addConstraint(Choco.eq(vars.get(diffVar),v));
    }

    /** add choco constraint **/
    private void addConstraint(Constraint c) {
        constraints.add(c);
        // cpmodel.addConstraint(c);
    }

    private static Constraint isTrue(IntegerExpressionVariable v1) {
        return Choco.eq(v1, 1);
    }

    private IntegerVariable getVar(String var) {
        return vars.get(var);
    }

    private boolean solve() {
        // add the constraints
        if (!solved) {
            for (Constraint c : constraints)
                cpmodel.addConstraint(c);
        }

        // show the problem
        if (absmodel.debug) {
            absmodel.println("## The constraints:");
            // ast.println(cpmodel.pretty());
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

    private boolean optimise(String var, Boolean minimise) {
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
                // Iterator<IntegerVariable> it = cpmodel.getIntVarIterator();
                // while (it.hasNext()) { // for all variables in the
                // constraints (model): round 2
                // IntegerVariable var2 = it.next();
                //
                // // If it is a parent to include, set
                // if (newParents.contains(var2.getName())) {
                // if (ast.debug)
                // ast.println("  "+var2+" (parent) -> 1");
                // cpmodel.addConstraint(Choco.eq(var2, 1));
                // }
                // }
            }
        }

        // show the problem
        if (absmodel.debug) {
            absmodel.println("## The constraints:");
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

        HashMap<String, Integer> result = new HashMap<>();

        Iterator<IntegerVariable> it = cpmodel.getIntVarIterator();
        while (it.hasNext()) {
            IntegerVariable var = it.next();
            result.put(var.getName(), solver.getVar(var).getVal());
        }
        return result;
    }

    /*
     * Return ALL solutions of the feature model
     */
    public Set<Map<String,Integer>> getSolutions() {
        Set<Map<String, Integer>> solutions = new HashSet<>();

        //int i=0;
        while(solveAgain()) {
            Map<String,Integer> sol = new HashMap<>();
            Iterator<IntegerVariable> it = cpmodel.getIntVarIterator();
            while (it.hasNext()) {
                IntegerVariable var = it.next();
                sol.put(var.getName(), solver.getVar(var).getVal());
            }
            //System.out.println(i++ + " sol: " + sol);
            solutions.add(sol);
        }
        return solutions;
    }

    //    public Set<Set<String>> getSolutionsFeaturesOnly() {
    //        Set<Set<String>> solutions = new HashSet<Set<String>>();
    //
    //        while(solveAgain()) {
    //            HashSet<String> sol = new HashSet<String>();
    //            Iterator<IntegerVariable> it = cpmodel.getIntVarIterator();
    //            while (it.hasNext()) {
    //                IntegerVariable var = it.next();
    //                if (solver.getVar(var).getVal() == 1) // We are dealing with features only, where 1 means TRUE
    //                    sol.add(var.getName());
    //            }
    //            solutions.add(sol);
    //        }
    //        return solutions;
    //    }

    public String getSolutionsAsString() {
        StringBuilder result = new StringBuilder();
        Set<Map<String,Integer>> solutions = getSolutions();
        int i=1;
        for (Map<String,Integer> sol : solutions) {
            result.append("------ " + (i++) + "------\n");
            for (String var : sol.keySet()) {
                if (absmodel.debug || !var.startsWith("$"))
                    result.append(var + " -> " + sol.get(var) + "\n");
            }
        }
        return result.toString();
    }

    private int maxValue(String optVar) {
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
        return getSolutionsAsString();
    }

    public String maximiseToString(String var) {
        if (absmodel.debug)
            absmodel.println("optimising " + var);
        optimise(var, false);
        return getSolutionsAsString();
    }

    public int maximiseToInt(String var) {
        if (absmodel.debug)
            absmodel.println("optimising " + var);
        optimise(var, false);
        return maxValue(var);
    }

    public List<String> checkSolutionWithErrors(Map<String, Integer> solution, Model model) {
        List<String> res = new ArrayList<>();
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

    private boolean checkSolution(Map<String, Integer> solution, Model model, CPModel m) {
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
            Set<String> newFeatures = new HashSet<>();
            Set<String> newParents = new HashSet<>();

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
        // it = cpmodel.getIntVarIterator();
        // while (it.hasNext()) {
        // IntegerVariable var = (IntegerVariable) it.next();
        // result = result + var.getName() + " -> "+s.getVar(var).getVal() +
        // "\n";
        // }
        // output.println("Trying:\n"+result);
        // }

        return s.checkSolution();
    }
}
