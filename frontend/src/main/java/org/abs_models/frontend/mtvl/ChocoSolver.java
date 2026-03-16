/*
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */

package org.abs_models.frontend.mtvl;

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
import org.abs_models.common.ListUtils;
import org.abs_models.frontend.analyser.SemanticConditionList;
import org.abs_models.frontend.ast.*;

import java.util.*;
import java.util.List;
import java.util.stream.Collectors;

public class ChocoSolver {

    private final CPModel cpmodel = new CPModel();
    private final CPSolver solver = new CPSolver();
    private boolean solved = false;
    private boolean newsol = false;
    final Map<String, IntegerVariable> vars = new HashMap<>();
    private final Map<String, Integer> defaultvals = new HashMap<>();
    private final Model absmodel;

    /**
     * Create a ChocoSolver instance and initialize its solver with
     * the model's feature constraints.
     */
    public static ChocoSolver fromModel(Model m) {
        ChocoSolver solver = new ChocoSolver(m);

        // new int variable for all int variables
        for (java.util.Map.Entry<String, BoundaryInt[]> entry : m.mtvlIntVariables().entrySet()) {
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
        for (String st : m.mtvlBoolVariables())
            solver.addBoolVar(st);
        for (String st : m.mtvlFeatures())
            solver.addBoolVar(st);

        solver.addConstraints(m); // is adding intvars to the model!
        return solver;
    }

    private ChocoSolver(Model m) {
        absmodel = m;
        if (m.debug)
            ChocoLogging.setVerbosity(Verbosity.DEFAULT);
        else
            ChocoLogging.setVerbosity(Verbosity.OFF);
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
    private IntegerVariable addIntVar(String name, int from, int to) {
        IntegerVariable v = Choco.makeIntVar(name, from, to);
        // addConstraint(Choco.geq(v,from)); // needed to include the variable
        // in the constraints to be solved.
        vars.put(name, v);
        defaultvals.put(name, from);
        if (absmodel.debug)
            absmodel.println("  adding Int var '" + name + "' (default -> " + from + ")");
        cpmodel.addVariable(v); // needed to include the variable in the constraints
        // to be solved.
        return v;
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

    private void addBoundedVar(String name, BoundaryInt b1, BoundaryInt b2) {
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
            int b = ((BoundaryVal) b1).getValue();
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
     * features must have been added already, as in fromModel.
     */
    public void addProductConstraints(Product m) {
        for (Feature f: m.getFeatures()) {
            addConstraint(isTrue(vars.get(f.getName())));
            for (AttrAssignment aa: f.getAttrAssignments()) {
                String fname = f.getName() + "." + aa.getName();
                if (vars.containsKey(fname))
                    addConstraint(Choco.eq(vars.get(fname), aa.getValue().getIntValue()));
            }
        }
    }

    /**
     * Add constraints of the given node to the solver.
     */
    private void addConstraints(ASTNode<?> s) {
        switch (s) {
            // ROOT: has to be present
            case CompilationUnit c: {
                for (int i = 0; i < c.getNumFeatureDecl(); i++) {
                    forceTrue(c.getFeatureDecl(i).getName());
                }
                for(int i = 0; i < c.getNumChild(); i++)
                    addConstraints(c.getChildNoTransform(i));
                break;
            }
            // FNODE
            case FNode f:
                addConstraints(f.getFeatureDecl());
                return;
            // FEATURE -> collect constraints, check cardinality, and check children.
            case FeatureDecl f: {
                AttrConstraints acl = f.getAttrConstraints();
                for(int i = 0; i < acl.getNumConstr(); i++)
                    addConstraint(constructConstraint(acl.getConstr(i)));
                if (f.hasGroup())
                    includeGroupConstraints(f.getGroup(),f.getName());
                return;
            }
            // FeatureExtension
            case FeatureExtension f: {
                AttrConstraints acl = f.getAttrConstraints();
                for(int i = 0; i < acl.getNumConstr(); i++)
                    addConstraint(constructConstraint(acl.getConstr(i)));
                if (f.hasGroup())
                    includeGroupConstraints(f.getGroup(),f.getName());
                return;
            }
            // GENERAL NODE: propagate
            case ASTNode<?> s1: {
                for(int i = 0; i < s1.getNumChild(); i++)
                    addConstraints(s.getChildNoTransform(i));
            }
        }
    }

    /**
     * Construct a solver constraint from a parsed mTVL constraint
     * expression.
     */
    private Constraint constructConstraint(Constr constr) {
        if (constr instanceof MEqualityExpr e) {
            // HACK: this checkType has side effects.  Need to type
            // check JUST the equality of expressions, to know if the
            // constraints should produce '==' or '<->'.  All other
            // type checking should be done after flattening.  See
            // FIXME in ErrorCheck.jadd:MEqualityExpr.checkType
            e.checkType(Types.BOOL, new SemanticConditionList());
        }
        return switch (constr) {
            // MExp
            case IfIn i -> Choco.implies(isTrue(vars.get(i.pname())), constructConstraint(i.getExpr()));
            case IfOut i -> Choco.implies(Choco.not(isTrue(vars.get(i.pname()))), constructConstraint(i.getExpr()));
            case Exclude e -> Choco.nand(vars.get(e.pname()), vars.get(e.getFeatVar().getFName()));
            case Require r -> Choco.implies(isTrue(vars.get(r.pname())), isTrue(vars.get(r.getFeatVar().getFName())));
            // SimpleExp
            case Variable v -> isTrue(vars.get(v.getFullName())); // Variable in boolean context (see also collectIntExpr)
            case MValue m -> switch (m.getValue()) {
                case BoolVal b -> b.getValue() ? Choco.TRUE : Choco.FALSE;
                default -> Choco.FALSE; // int values are handled in collectIntExpr
            };
            // MUnary
            case MNegExp m -> Choco.not(constructConstraint(m.getOperand()));
            case MMinusExp m -> Choco.FALSE; // int values are handled in collectIntExpr
            // MBinary
            // /\
            case MAndBoolExp m -> Choco.and(constructConstraint(m.getLeft()),constructConstraint(m.getRight()));
            // \/
            case  MOrBoolExp m -> Choco.or(constructConstraint(m.getLeft()),constructConstraint(m.getRight()));
            // =>
            case MImpliesExp m -> Choco.implies(constructConstraint(m.getLeft()),constructConstraint(m.getRight()));
            // <=>
            case  MEquivExp m -> Choco.ifOnlyIf(constructConstraint(m.getLeft()),constructConstraint(m.getRight()));
            // <
            case MLTExp m -> Choco.lt(collectIntExpr(m.getLeft()),collectIntExpr(m.getRight()));
            // >
            case MGTExp m -> Choco.gt(collectIntExpr(m.getLeft()),collectIntExpr(m.getRight()));
            // <=
            case MLTEQExp m -> Choco.leq(collectIntExpr(m.getLeft()),collectIntExpr(m.getRight()));
            // >=
            case MGTEQExp m -> Choco.geq(collectIntExpr(m.getLeft()),collectIntExpr(m.getRight()));
            // EXP: EqualityExpr
            case MEqExp m -> (m.isInt)
                ? Choco.eq(collectIntExpr(m.getLeft()),collectIntExpr(m.getRight()))
                : Choco.ifOnlyIf(constructConstraint(m.getLeft()),constructConstraint(m.getRight()));
            case MNotEqExp m -> (m.isInt)
                ? Choco.neq(collectIntExpr(m.getLeft()),collectIntExpr(m.getRight()))
                : Choco.nand(constructConstraint(m.getLeft()),constructConstraint(m.getRight()));
            case MArithmeticExpr m -> Choco.FALSE; // int values are handled in collectIntExpr
            // (rudi) shut up the compiler -- I can't see which case isn't covered by the above :(
            default -> Choco.FALSE;
        };
    }

    /**
     * Construct a solver integer variable or expression.  Called when
     * we know we are in an int context while generating a constraint.
     */
    private IntegerExpressionVariable collectIntExpr(ASTNode<?> mexp) {
        // argument can be MExp or Value
        return switch (mexp) {
            // EXPRESSIONS
            // EXP:VARS
            case Variable v -> vars.get(v.getFullName());
            //EXP:VALUES
            // > ????
            case MValue m -> switch (m.getValue()) {
                case IntVal v -> Choco.constant(v.getValue());
                case BoolVal b -> Choco.constant(b.getValue() ? 1 : 0);
                default -> Choco.constant(0); // should never happen
            };
            // EXP: AddExp
            case MAddAddExp m -> Choco.plus(collectIntExpr(m.getLeft()),collectIntExpr(m.getRight()));
            case MSubAddExp m -> Choco.minus(collectIntExpr(m.getLeft()),collectIntExpr(m.getRight()));
            // EXP: MultExp
            case  MMultMultExp m -> Choco.mult(collectIntExpr(m.getLeft()),collectIntExpr(m.getRight()));
            case  MDivMultExp m -> Choco.div(collectIntExpr(m.getLeft()),collectIntExpr(m.getRight()));
            case  MModMultExp m -> Choco.mod(collectIntExpr(m.getLeft()),collectIntExpr(m.getRight()));
                // EXP: Unary
            case  MMinusExp m -> Choco.neg(collectIntExpr(m.getOperand()));
            // EXP: GENERAL (always overwritten when the program type-checks)
            default -> null;
        };
    }

    private void includeGroupConstraints(Group g, String varName) {
        IntegerVariable fvar = vars.get(varName);
        // Note: FNodes of type OptFeat are only allowed inside AllOf
        // groups (ensured by the type checker), so in all other cases
        // allFeatures and mandatoryFeatures will be identical.
        List<FNode> allFeatures = ListUtils.toJavaList(g.getFNodes());
        List<FNode> mandatoryFeatures = allFeatures.stream()
            .filter(fnode -> !(fnode instanceof OptFeat))
            .toList();
        allFeatures.forEach(fnode -> {
            IntegerVariable v = vars.get(fnode.getFeatureDecl().getName());
            addConstraint(Choco.implies(isTrue(v), isTrue(fvar)));
            addConstraints(fnode);
        });
        // only in case of AllOf, this does not contain optional
        // feature variables; in all other cases this contains all
        // feature variables.
        IntegerVariable[] feats = mandatoryFeatures.stream()
            .map(fnode -> vars.get(fnode.getFeatureDecl().getName()))
            .toArray(IntegerVariable[]::new);
        // n1 <= $f1 + ... + $fn <= n2
        if (g.getCard() instanceof OneOf)
            // f ->  #feats = 1
            addConstraint(Choco.implies(isTrue(fvar),
                Choco.eq(Choco.sum(feats),1)));
        else if (g.getCard() instanceof AllOf)
            // f ->  #feats = nfeats
            addConstraint(Choco.implies(isTrue(fvar),
                // note that in this case we only check the mandatory features
                Choco.eq(Choco.sum(feats), feats.length)));
        else if (g.getCard() instanceof Minim)
            // f ->  #feats >= from
            addConstraint(Choco.implies(
                isTrue(fvar),
                Choco.geq(Choco.sum(feats),((Minim) g.getCard()).getCFrom())));
        else {
            // f ->  to >= #feats >= from
            // Note: OneOf is translated into CRange(1, 1)
            addConstraint(Choco.implies(
                isTrue(fvar),
                Choco.geq(Choco.sum(feats),((CRange) g.getCard()).getCFrom())));
            addConstraint(Choco.implies(
                isTrue(fvar),
                Choco.leq(Choco.sum(feats),((CRange) g.getCard()).getCTo())));
        }
    }

    /**
     * Calculate the maximum number of features that a product can
     * have.
     */
    public static String calculateMaxProductFeatures(Model m) {
        ChocoSolver s = ChocoSolver.fromModel(m);
        IntegerVariable res = s.addIntVar("noOfFeatures", 0, 50);
        IntegerExpressionVariable v = Choco.ZERO;
        for(String fname: m.mtvlFeatures()){
            if (s.vars.containsKey(fname))
                v = Choco.plus(v, s.vars.get(fname));
        }
        s.addConstraint(Choco.eq(res, v));
        if (m.debug)
            m.println("optimising noOfFeatures");
        s.optimise("noOfFeatures", false);
        return s.getSolutionsAsString();
    }

    /**
     * Calculate the smallest feature configuration that satisfies the
     * given product.
     */
    public static String calculateMinFeaturesOfProduct(Model m, Product p) {
        ChocoSolver s = ChocoSolver.fromModel(m);
        s.addIntVar("difference", 0, 50);
        s.addDiffConstraint(m, p, "difference");
        return s.minimiseToString("difference");
    }


    private void addDiffConstraint(Model m, Product p, String diffVar) {
        List<Feature> productFeatures = ListUtils.toJavaList(p.getFeatures());
        List<String> unusedFeatures = m.mtvlFeatures()
            .stream()
            .filter(fname -> productFeatures
                .stream()
                .anyMatch(f -> f.getName().equalsIgnoreCase(fname)))
            .collect(Collectors.toUnmodifiableList());

        IntegerExpressionVariable v = Choco.ZERO;
        for (Feature f: productFeatures) {
            v = Choco.plus(v, Choco.abs(Choco.minus(vars.get(f.getName()), 1)));
        }

        for(String fname: unusedFeatures) {
            v = Choco.plus(v, vars.get(fname));
        }
        addConstraint(Choco.eq(vars.get(diffVar),v));
    }

    private static Constraint isTrue(IntegerExpressionVariable v1) {
        return Choco.eq(v1, 1);
    }

    /** add choco constraint **/
    private void addConstraint(Constraint c) {
        cpmodel.addConstraint(c);
    }

    private boolean solve() {
        // show the problem
        if (absmodel.debug) {
            absmodel.println("## The constraints:");
            // ast.println(cpmodel.pretty());
            for (Iterator<Constraint> it = cpmodel.getConstraintIterator(); it.hasNext(); ) {
                Constraint c = it.next();
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

    private void optimise(String var, Boolean minimise) {
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
            for (Iterator<Constraint> it = cpmodel.getConstraintIterator(); it.hasNext(); ) {
                Constraint c = it.next();
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
            }
    }

    /**
     * Return the number of solutions (possible products) according to
     * the feature model.
     */
    public int countSolutions() {
        if (absmodel.debug) {
            absmodel.print("## The constraints:");
            absmodel.println(cpmodel.pretty());
        }

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

    /**
     * Return a solution for the feature model.
     */
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

    /**
     * Return a textual representation of all possible solutions.
     */
    public String getSolutionsAsString() {
        StringBuilder result = new StringBuilder();
        Set<Map<String,Integer>> solutions = getSolutions();
        int i=1;
        for (Map<String,Integer> sol : solutions) {
            result.append("------ ").append(i++).append(" ------\n");
            for (String var : sol.keySet()) {
                result.append(var).append(" -> ").append(sol.get(var)).append("\n");
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

    /**
     * Check that the feature requirements of a product can be
     * fulfilled against the mTVL model of the product line.
     *
     * Note that this is a static method because it uses a separate
     * ChocoSolver instance.
     *
     * @param product the product to be checked
     * @param model the ABS model
     * @return a (potentially empty) list of constraints that are
     *  violated by the product's feature requirements
     */
    public static List<String> checkProduct(Product product, Model model) {

        ChocoSolver s = fromModel(model);

        // Calculate assignment of features, feature attributes
        Map<String,Integer> configuration = new HashMap<String,Integer>();
        for(Feature f : product.getFeatures()) {
            configuration.put(f.getName(), Integer.valueOf(1));
            for(AttrAssignment attr : f.getAttrAssignments()) {
                configuration.put(f.getName() + "." + attr.getName(), attr.getValue().getIntValue());
            }
        }
        List<String> res = new ArrayList<>();
        // check first for limits of variables
        for (IntegerVariable v : s.vars.values()) {
            CPModel m = new CPModel();
            m.addVariable(v);
            if (!checkConfiguration(configuration, model, m, s.defaultvals))
                res.add(v.toString());
        }
        // now check all explicit constraints
        for (Iterator<Constraint> it = s.cpmodel.getConstraintIterator(); it.hasNext(); ) {
            Constraint c = it.next();
            CPModel m = new CPModel();
            m.addConstraint(c);
            if (!checkConfiguration(configuration, model, m, s.defaultvals))
                res.add(prettyConst(c));
        }
        return res;
    }

    private static boolean checkConfiguration(Map<String, Integer> configuration, Model model, CPModel m, Map<String, Integer> defaultvals) {
        // Read the model
        CPSolver s = new CPSolver();
        s.read(m);

        if (model.debug)
            model.println("configuration to check:\n" + configuration);

        // HashMap<String,Integer> selection = new HashMap<String,Integer>();

        Iterator<IntegerVariable> it = m.getIntVarIterator();
        try {
            // aux variables
            int val;
            Set<String> newFeatures = new HashSet<>();
            Set<String> newParents = new HashSet<>();

            if (model.debug)
                model.println("Adding new values:");
            while (it.hasNext()) { // for all variables in the constraints
                // (model): round 1
                IntegerVariable var = it.next();
                // IF used variable is present in the configuration, update it!
                if (configuration.containsKey(var.getName())) {
                    val = configuration.get(var.getName());
                    if (model.debug)
                        model.println("  " + var + " -> " + val);
                    s.getVar(var).setVal(val);
                    // Possible feature name -- include later the parents.
                    if (val == 1)
                        newFeatures.add(var.getName());
                }
            }
            // add parents of features from the configuration that are not in the
            // constraints (model)
            for (Map.Entry<String, Integer> entry : configuration.entrySet()) {
                if (entry.getValue() == 1)
                    if (!entry.getKey().contains("."))
                        newFeatures.add(entry.getKey());
            }

            // collect parents of 'newFeatures'
            if (model != null)
                model.collectParents(newFeatures, newParents);
            // add newParents and default values to the configuration
            it = m.getIntVarIterator();
            while (it.hasNext()) { // for all variables in the constraints
                // (model): round 2
                IntegerVariable var = it.next();

                // If it is a parent to include, set
                if (newParents.contains(var.getName())) {
                    if (model.debug)
                        model.println("  " + var + " (parent) -> 1");
                    s.getVar(var).setVal(1);
                }
                // ELSE use default value
                else if (!configuration.containsKey(var.getName())) {
                    if (defaultvals.containsKey(var.getName())) {
                        // By default, unrefered features & attributes are false
                        int defval = defaultvals.get(var.getName());
                        if (model.debug)
                            model.println("  " + var.getName() + " (default) -> " + defval);
                        s.getVar(var).setVal(defval);
                    } else {
                        if (model.debug)
                            model.println("  " + var.getName() + " (default) -> 0");
                        s.getVar(var).setVal(0);
                    }
                }
            }
        } catch (ContradictionException e1) {
            if (model.debug)
                System.err.println("$$$ Contradiction found... $$$");
        }
        // Catch-all
        catch (Exception e1) {
            // Catch-all
            if (model.debug) {
                System.err.println("$$$ Failed to check configuration... $$$");
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

    // Adds parenthesis for readability if there are *spaces* in the string.
    private static String mbParenthesis(String s) {
        if (s.contains(" "))
            return "(" + s + ")";
        else
            return s;
    }

    private static String prettyConst(Constraint c) {
        // System.out.println( "["+c.getClass()+"] "+c.pretty());
        if (c instanceof MetaConstraint<?> mc) {
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
        if (c instanceof ComponentConstraint cc) {
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
        if (v instanceof IntegerExpressionVariable exp) {
            if (exp.getOperator().name().equals("SUM")) {
                if (exp.getNbVars() > 0) {
                    StringBuilder res = new StringBuilder();
                    res.append(prettyVar(exp.getVariable(0)));
                    for (int i = 1; i < exp.getNbVars(); i++)
                        res.append(" + ").append(prettyVar(exp.getVariable(i)));
                    return res.toString();
                } else
                    return "0";
            }
        }
        if (v instanceof IntegerVariable iv && iv.isBoolean()) {
                return iv.getName();
        }
        return v.pretty();
    }

}
