/*
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */

package org.abs_models.frontend.mtvl;

import org.abs_models.common.ListUtils;
import org.abs_models.frontend.analyser.SemanticConditionList;
import org.abs_models.frontend.ast.*;
import org.chocosolver.solver.Solution;
import org.chocosolver.solver.Solver;
import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.expression.discrete.arithmetic.ArExpression;
import org.chocosolver.solver.expression.discrete.relational.ReExpression;
import org.chocosolver.solver.variables.BoolVar;
import org.chocosolver.solver.variables.IntVar;
import org.chocosolver.util.ESat;

import java.util.*;
import java.util.List;

/// This class analyzes a model's software product line (SPL)
/// definition.  It is used by the type checker and the `checkspl`
/// subcommand.  It translates the software product line into a set of
/// constraints and uses a constraint solver to check for consistency
/// of product definitions against the feature constraints of the SPL.
/// 
/// The feature model translation to constraints is inspired by
/// https://link.springer.com/chapter/10.1007/978-3-642-25271-6_11 but
/// note the different handling of optional children of allof groups.
/// 
/// - One Boolean variable per feature, named after the feature.
/// - One Int variable per feature attribute, named feature + "." +
///   attribute.  Boolean attributes are translated into 1 / 0.
///   String attributes are ignored.
/// 
/// - All top-level (root) features are true.
/// - For features inside a group: the (presence of the) feature
///   implies the parent (group) feature.
/// - For `AllOf` groups: the parent implies all mandatory child
///   features.
/// - For `OneOf` groups: the parent implies exactly one child feature
///   (optional children forbidden).
/// - For `CRange`, `Minim` groups: the parent implies the number of
///   child features (optional children forbidden).
public class ChocoSolver {

    private final org.chocosolver.solver.Model cpmodel = new org.chocosolver.solver.Model();

    /// This map stores all solver variables.  Naming convention:
    /// - Feature name (alphanumeric string beginning with upper case):
    ///   boolean variable, true if feature present
    /// - Feature name + "." + attribute name (alphanumeric string):
    ///   feature attribute value
    final Map<String, IntVar> vars = new HashMap<>();

    /// This map stores feature variables; these are boolean variables
    /// telling whether the feature is present or not.
    final Map<String, BoolVar> featureVars = new HashMap<>();

    /// This map stores default values for feature and attribute
    /// variables.  These values are used to initialize variables while
    /// checking a product definition that does not provide its own
    /// value for the given variable.
    private final Map<String, Integer> defaultvals = new HashMap<>();

    private final Model absmodel;

    private ChocoSolver(Model m) {
        absmodel = m;
    }

    /**
     * Create a ChocoSolver instance and initialize its solver with
     * the model's feature constraints.
     */
    public static ChocoSolver fromModel(Model m) {
        ChocoSolver solver = new ChocoSolver(m);

        for (java.util.Map.Entry<String, BoundaryInt[]> entry : m.mtvlIntVariables().entrySet()) {
            String name = entry.getKey();
            if (entry.getValue().length == 2) {
                BoundaryInt b1 = entry.getValue()[0];
                BoundaryInt b2 = entry.getValue()[1];
                solver.internBoundedVar(name, b1, b2);
            } else {
                // FIXME: what about an IntListMType with exactly two entries?
                solver.internSetVar(name, entry.getValue());
            }
        }
        for (String name : m.mtvlBoolVariables()) {
            solver.internBoolVar(name);
            if (m.debug)
                m.println("  adding Bool var '" + name + "' (default False)");
        }
        for (String name : m.mtvlFeatures()) {
            BoolVar v = solver.internBoolVar(name);
            solver.featureVars.put(name, v);
            if (m.debug)
                m.println("  adding Feature var '" + name + "' (default False)");
        }

        solver.addConstraints(m); // is adding intvars to the model!
        return solver;
    }

    /**
     * Add bool variable named `name` if it doesn't exist yet.
     */
    private BoolVar internBoolVar(String name) {
        if (vars.containsKey(name)) return vars.get(name).asBoolVar();
        BoolVar v = cpmodel.boolVar(name);
        vars.put(name, v);
        defaultvals.put(name, 0);
        return v;
    }

    /**
     * Add int variable named `name` if it doesn't exist yet.
     *
     * @param name name of the variable to be added
     * @param from lowerlimit of the domain of the variable
     * @param to upper limit of the domain of the variable
     **/
    private IntVar internIntVar(String name, int from, int to) {
        if (vars.containsKey(name)) return vars.get(name);
        IntVar v = cpmodel.intVar(name, from, to);
        // addConstraint(Choco.geq(v,from)); // needed to include the variable
        // in the constraints to be solved.
        vars.put(name, v);
        defaultvals.put(name, from);
        if (absmodel.debug)
            absmodel.println("  adding Int var '" + name + "' (" + from + "-" + to + ", default " + from + ")");
        return v;
    }

    /**
     * Add variable `name` if it doesn't exist yet.
     */
    private IntVar internBoundedVar(String name, BoundaryInt b1, BoundaryInt b2) {
        if (vars.containsKey(name)) return vars.get(name); // variables are unique
        IntVar v;
        if (b1 instanceof Limit)
            if (b2 instanceof Limit) {
                v = cpmodel.intVar(name, IntVar.MIN_INT_BOUND, IntVar.MAX_INT_BOUND);
                vars.put(name, v);
                defaultvals.put(name, 0);
                if (absmodel.debug)
                    absmodel.println("  adding Int var '" + name + "' (unbounded, default 0)");
            } else {
                int b = ((BoundaryVal) b2).getValue();
                v = cpmodel.intVar(name, IntVar.MIN_INT_BOUND, b);
                vars.put(name, v);
                defaultvals.put(name, b);
                if (absmodel.debug)
                    absmodel.println("  adding Int var '" + name + "' (less than " + b + ", default " + b + ")");
            }
        else if (b2 instanceof Limit) {
            int b = ((BoundaryVal) b1).getValue();
            v = cpmodel.intVar(name, b, IntVar.MAX_INT_BOUND);
            vars.put(name, v);
            defaultvals.put(name, b);
            if (absmodel.debug)
                absmodel.println("  adding Int var '" + name + "' (greater than " + b + ", default " + b + ")");
            }
        else {
            v = internIntVar(name, ((BoundaryVal) b1).getValue(), ((BoundaryVal) b2).getValue());
        }
        return v;
    }

    /**
     * Add variable `name` if it doesn't exist yet.
     */
    private IntVar internSetVar(String name, BoundaryInt[] bs) {
        if (vars.containsKey(name)) return vars.get(name);
        int bsize = bs.length - 1;
        int[] vals = new int[bsize];
        // addSetVar only called if bs has only BoundaryVals
        for (int i=0; i < bsize; i++) {
            vals[i] = ((BoundaryVal) bs[i+1]).getValue(); // drop first value - repeated
        }
        IntVar v = cpmodel.intVar(name, vals);
        vars.put(name, v);
        defaultvals.put(name, vals[0]); // vals has at least 1 element! (by the parser constraints)
        if (absmodel.debug)
            absmodel.println("  adding IntSet var '" + name + "' (" + Arrays.toString(vals) + ", default " + vals[0] + ")");
        return v;
    }

    /**
     * Add all feature constraints of a product.  Note that the
     * features must have been added already; this is done in
     * fromModel.
     */
    public void addProductConstraints(Product m) {
        for (Feature f: m.getFeatures()) {
            addConstraint(isTrue(featureVars.get(f.getName())));
            for (AttrAssignment aa: f.getAttrAssignments()) {
                String fname = f.getName() + "." + aa.getName();
                if (vars.containsKey(fname)) {
                    switch (aa.getValue()) {
                        case IntVal iv:
                            addConstraint(cpmodel.arithm(vars.get(fname), "=", aa.getValue().getIntValue()));
                            break;
                        case BoolVal bv:
                            addConstraint(vars.get(fname).asBoolVar().eq(bv.getIntValue()));
                            break;
                        default:
                            System.err.println("Warning: ignoring constraint for non-Int, non-Bool feature variable " + fname);
                            break;
                    }
                } else {
                    System.err.println("Warning: trying to set feature variable " + fname + " which is not defined in the feature");
                }
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
                for (FeatureDecl root : c.getFeatureDecls()) {
                    // These are the "root" features, as in `root XXX { ... }` -- these must be true
                    String name = root.getName();
                    addConstraint(isTrue(featureVars.get(name)));
                    defaultvals.put(name, 1);
                }
                for(int i = 0; i < c.getNumChild(); i++) {
                    addConstraints(c.getChildNoTransform(i));
                }
                break;
            }
            // FNODE
            case OptFeat f:
                addConstraints(f.getFeatureDecl());
                return;
            case MandFeat f:
                addConstraints(f.getFeatureDecl());
                return;
            // FEATURE -> collect constraints, check cardinality, and check children.
            case FeatureDecl f: {
                for (Constr c : f.getAttrConstraints().getConstrs()) {
                    addConstraint(constructExpression(c));
                }
                if (f.hasGroup()) {
                    includeGroupConstraints(f.getGroup(), f.getName());
                }
                return;
            }
            // FeatureExtension
            case FeatureExtension f: {
                for (Constr c : f.getAttrConstraints().getConstrs()) {
                    addConstraint(constructExpression(c));
                }
                if (f.hasGroup()) {
                    includeGroupConstraints(f.getGroup(), f.getName());
                }
                return;
            }
            // GENERAL NODE: propagate
            case ASTNode<?> s1: {
                for(int i = 0; i < s1.getNumChild(); i++) {
                    addConstraints(s.getChildNoTransform(i));
                }
            }
        }
    }

    /**
     * Construct a solver arithmetic expression from a mTVL constraint
     * expression AST.
     */
    private ReExpression constructExpression(Constr constr) {
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
            // i => exp
            case IfIn i -> isTrue(vars.get(i.pname())).imp(constructExpression(i.getExpr()));
            // ¬i => exp
            case IfOut i -> isTrue(vars.get(i.pname())).not().imp(constructExpression(i.getExpr()));
            // e ⊼ e_feat -- ¬e ∨ ¬e_feat
            case Exclude e -> isTrue(vars.get(e.pname())).not().or(isTrue(vars.get(e.getFeatVar().getFName())).not());
            // e => e_feat
            case Require r -> isTrue(vars.get(r.pname())).imp(isTrue(vars.get(r.getFeatVar().getFName())));
            // SimpleExp
            case Variable v -> isTrue(vars.get(v.getFullName())); // Variable in boolean context (see also collectIntExpr)
            case MValue m -> switch (m.getValue()) {
                case BoolVal b -> b.getValue() ? cpmodel.boolVar(true) : cpmodel.boolVar(false);
                default -> cpmodel.boolVar(false); // int values are handled in collectIntExpr
            };
            // MUnary
            case MNegExp m -> constructExpression(m.getOperand()).not();
            case MMinusExp m -> cpmodel.boolVar(false); // int values are handled in collectIntExpr
            // MBinary
            // /\
            case MAndBoolExp m -> constructExpression(m.getLeft()).and(constructExpression(m.getRight()));
            // \/
            case  MOrBoolExp m -> constructExpression(m.getLeft()).or(constructExpression(m.getRight()));
            // =>
            case MImpliesExp m -> constructExpression(m.getLeft()).imp(constructExpression(m.getRight()));
            // <=>
            case  MEquivExp m -> constructExpression(m.getLeft()).iff(constructExpression(m.getRight()));
            // <
            case MLTExp m -> collectIntExpr(m.getLeft()).lt(collectIntExpr(m.getRight()));
            // >
            case MGTExp m -> collectIntExpr(m.getLeft()).gt(collectIntExpr(m.getRight()));
            // <=
            case MLTEQExp m -> collectIntExpr(m.getLeft()).le(collectIntExpr(m.getRight()));
            // >=
            case MGTEQExp m -> collectIntExpr(m.getLeft()).ge(collectIntExpr(m.getRight()));
            // EXP: EqualityExpr
            case MEqExp m -> (m.isInt)
                ? collectIntExpr(m.getLeft()).eq(collectIntExpr(m.getRight()))
                : constructExpression(m.getLeft()).iff(constructExpression(m.getRight()));
            case MNotEqExp m -> (m.isInt)
                ? collectIntExpr(m.getLeft()).ne(collectIntExpr(m.getRight()))
                : constructExpression(m.getLeft()).not().or(constructExpression(m.getRight()).not());
            case MArithmeticExpr m -> cpmodel.boolVar(false); // int values are handled in collectIntExpr
            // (rudi) shut up the compiler -- I can't see which case isn't covered by the above :(
            default -> cpmodel.boolVar(false);
        };
    }

    /**
     * Construct a solver integer variable or expression.  Called when
     * we know we are in an int context while generating a constraint.
     */
    private ArExpression collectIntExpr(ASTNode<?> mexp) {
        // argument can be MExp or Value
        return switch (mexp) {
            // EXPRESSIONS
            // EXP:VARS
            case Variable v -> vars.get(v.getFullName());
            //EXP:VALUES
            // > ????
            case MValue m -> switch (m.getValue()) {
                case IntVal v -> cpmodel.intVar(v.getValue());
                case BoolVal b -> cpmodel.boolVar(b.getValue());
                default -> cpmodel.intVar(-1); // Someone does expressions on Strings, maybe?
            };
            // EXP: AddExp
            case MAddAddExp m -> collectIntExpr(m.getLeft()).add(collectIntExpr(m.getRight()));
            case MSubAddExp m -> collectIntExpr(m.getLeft()).sub(collectIntExpr(m.getRight()));
            // EXP: MultExp
            case  MMultMultExp m -> collectIntExpr(m.getLeft()).mul(collectIntExpr(m.getRight()));
            case  MDivMultExp m -> collectIntExpr(m.getLeft()).div(collectIntExpr(m.getRight()));
            case  MModMultExp m -> collectIntExpr(m.getLeft()).mod(collectIntExpr(m.getRight()));
                // EXP: Unary
            case  MMinusExp m -> collectIntExpr(m.getOperand()).neg();
            // EXP: GENERAL (always overwritten when the program type-checks)
            default -> cpmodel.intVar(-1); // should never happen
        };
    }

    private void includeGroupConstraints(Group g, String varName) {
        BoolVar fvar = featureVars.get(varName);
        // Note: FNodes of type OptFeat are only allowed inside AllOf
        // groups (ensured by the type checker), so in all other cases
        // allFeatures and mandatoryFeatures will be identical.
        List<FNode> allFeatures = ListUtils.toJavaList(g.getFNodes());
        List<FNode> mandatoryFeatures = allFeatures.stream()
            .filter(fnode -> !(fnode instanceof OptFeat))
            .toList();
        allFeatures.forEach(fnode -> {
            BoolVar v = featureVars.get(fnode.getFeatureDecl().getName());
            addConstraint(v.imp(fvar));
            addConstraints(fnode);
        });
        // Only in case of AllOf, this does not contain optional
        // feature variables; in all other cases this contains all
        // feature variables.
        BoolVar[] feats = mandatoryFeatures.stream()
            .map(fnode -> featureVars.get(fnode.getFeatureDecl().getName()))
            .toArray(BoolVar[]::new);
        // n1 <= $f1 + ... + $fn <= n2
        if (feats.length > 0) {
            switch (g.getCard()) {
                case OneOf o: {
                    addConstraint(fvar.imp(cpmodel.sum(feats, "=", 1).reify()));
                    break;
                }
                case AllOf a: {
                    // f ->  #feats = #mandatory feats
                    addConstraint(fvar.imp(cpmodel.sum(feats, "=", feats.length).reify()));
                    break;
                }
                case Minim m: {
                    // f ->  #feats >= from\
                    addConstraint(fvar.imp(cpmodel.sum(feats, ">=", m.getCFrom()).reify()));
                    break;
                }
                case CRange r: {
                    // f ->  to >= #feats >= from
                    addConstraint(fvar.imp(cpmodel.sum(feats, ">=", r.getCFrom()).reify()));
                    addConstraint(fvar.imp(cpmodel.sum(feats, "<=", r.getCTo()).reify()));
                    break;
                }
                default:
                    // pacify compiler; all cases enumerated above
                    break;
            }
        }
    }

    /**
     * Calculate the maximum number of features that a product can
     * have.
     */
    public static String calculateMaxProductFeatures(Model m) {
        ChocoSolver s = ChocoSolver.fromModel(m);
        IntVar res = s.internIntVar("noOfFeatures", 0, 50);
        IntVar[] vv = m.mtvlFeatures().stream()
            .filter(s.vars::containsKey)
            .map(s.vars::get)
            .toArray(IntVar[]::new);
        s.addConstraint(s.cpmodel.sum(vv, "=", res));
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
        s.internIntVar("difference", 0, 50);
        s.addDiffConstraint(m, p, "difference");
        return s.minimiseToString("difference");
    }


    private void addDiffConstraint(Model m, Product p, String diffVar) {
        List<IntVar> productFeatures = ListUtils.toJavaList(p.getFeatures())
            .stream()
            .map(Feature::getName)
            .map(vars::get)
            .toList();
        List<IntVar> unusedFeatures = m.mtvlFeatures()
            .stream()
            .filter(fname -> productFeatures
                .stream()
                .anyMatch(f -> f.getName().equalsIgnoreCase(fname)))
            .map(vars::get)
            .toList();

        ArExpression e = cpmodel.intVar(0);
        for (IntVar f: productFeatures) {
            // sum up all deselected product variables
            e = e.add(f.asBoolVar().not());
        }
        for(IntVar f: unusedFeatures) {
            // add all deselected features
            e = e.add(f);
        }
        addConstraint(vars.get(diffVar).eq(e));
    }

    private ReExpression isTrue(IntVar v1) {
        return v1.eq(1);
    }

    /** add choco constraint **/
    private void addConstraint(Constraint c) {
        if (absmodel.debug) {
            System.out.println("  adding constraint " + c);
        }
        c.post();
    }

    private void addConstraint(ReExpression r) {
        if (absmodel.debug) {
            System.out.println("  adding constraint expression " + r);
        }
        r.post();
    }

    private Solution optimise(String var, Boolean minimise) {
        // Impose the feature name to be present
        if (var.contains(".")) {
            String feat = var.split("\\.")[0];
            if (absmodel.debug)
                absmodel.println("## including feature '" + feat + "'"); // and its parents.");
            if (vars.containsKey(feat)) {
                if (absmodel.debug)
                    absmodel.println("  " + feat + " (selected) -> 1");
                addConstraint(isTrue(vars.get(feat)));

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
            for (Constraint c : cpmodel.getCstrs()) {
                absmodel.println(c.toString());
            }
            absmodel.println("-----");
        }

        // Read the model
        Solver solver = cpmodel.getSolver();
        // Minmise the model, if possible
        if (vars.containsKey(var)) {
            return solver.findOptimalSolution(vars.get(var),
                minimise ? org.chocosolver.solver.Model.MINIMIZE : org.chocosolver.solver.Model.MAXIMIZE);
        } else {
            return null;
        }
    }

    /**
     * Return the number of solutions (possible products) according to
     * the feature model.
     */
    public int countSolutions() {
        if (absmodel.debug) {
            absmodel.print("## The constraints:");
            for (Constraint c : cpmodel.getCstrs()) {
                absmodel.println(c.toString());
            }
            absmodel.println("-----");
        }
        return cpmodel.getSolver().findAllSolutions().size();
    }

    /**
     * Return a solution for the feature model.
     */
    public Map<String, Integer> getSolution() {
        HashMap<String, Integer> result = new HashMap<>();
        if (cpmodel.getSolver().solve()) {
            for (Map.Entry<String, IntVar> e : vars.entrySet()) {
                if (e.getValue().isInstantiated()) {
                    result.put(e.getKey(), e.getValue().getValue());
                }
            }
        }
        return result;
    }

    /*
     * Return ALL solutions of the feature model
     */
    public Set<Map<String,Integer>> getSolutions() {
        Set<Map<String, Integer>> solutions = new HashSet<>();
        while (cpmodel.getSolver().solve()) {
            Map<String,Integer> sol = new HashMap<>();
            for (Map.Entry<String, IntVar> e : vars.entrySet()) {
                if (e.getValue().isInstantiated()) {
                    sol.put(e.getKey(), e.getValue().getValue());
                }
            }
            solutions.add(sol);
        }
        return solutions;
    }

    /**
     * Return a textual representation of all possible solutions.
     */
    public String getSolutionsAsString() {
        StringBuilder result = new StringBuilder();
        int i=1;
        for (Map<String,Integer> sol : getSolutions()) {
             result.append("------ ").append(i++).append(" ------\n");
            for (String var : sol.keySet()) {
                result.append(var).append(" -> ").append(sol.get(var)).append("\n");
            }
        }
        return result.toString();
    }

    public String minimiseToString(String var) {
        optimise(var, true);
        return getSolutionsAsString();
    }

    /**
     * Check that the feature requirements of a product can be
     * fulfilled against the mTVL model of the product line.
     *
     * <p>Note that this is a static method because it uses a separate
     * ChocoSolver instance.
     *
     * @param product the product to be checked
     * @param model the ABS model
     * @return a (potentially empty) list of constraints that are
     *  violated by the product's feature requirements
     */
    public static List<String> checkProduct(Product product, Model model) {

        if (model.debug) {
            model.println("Preparing CSP model:");
        }
        ChocoSolver s = fromModel(model);

        // Calculate assignment of features, feature attributes
        Map<String,Integer> configuration = new HashMap<>();
        for(Feature f : product.getFeatures()) {
            configuration.put(f.getName(), 1);
            for(AttrAssignment attr : f.getAttrAssignments()) {
                configuration.put(f.getName() + "." + attr.getName(), attr.getValue().getIntValue());
            }
        }

        // // check first for limits of variables
        // for (IntVar v : s.vars.values()) {
        //     org.chocosolver.solver.Model m = new org.chocosolver.solver.Model();
        //     m.intVar(v);
        //     if (!checkConfiguration(configuration, model, m, s.defaultvals))
        //         result.add(v.toString());
        // }

        // // now check all explicit constraints
        // for (Constraint c : s.cpmodel.getCstrs()) {
        //     org.chocosolver.solver.Model m = new org.chocosolver.solver.Model();
        //     m.addConstraint(c);
        //     if (!checkConfiguration(configuration, model, m, s.defaultvals))
        //         result.add(c.toString());
        // }
        if (checkConfiguration(configuration, model, s.cpmodel, s.defaultvals) == null) {
            // TODO: return something here -- solution is null but how can we get at the violated constraints?
            return Arrays.stream(s.cpmodel.getCstrs())
                .filter(c -> c.isSatisfied() != ESat.TRUE) // FALSE or UNDEFINED
                .map(Constraint::toString)
                .toList();
        } else {
            return List.of();
        }
    }

    /**
     * Note: modifies {@code cpmodel}
     *
     * @return a solution if it exists, or null.
     */
    private static Solution checkConfiguration(Map<String, Integer> configuration,
                                              Model absmodel,
                                              org.chocosolver.solver.Model cpmodel,
                                              Map<String, Integer> defaultvals)
    {
        if (absmodel.debug) {
            absmodel.println("Configuration to check: " + configuration);
            absmodel.println("Model to check against: " + cpmodel);
        }

        try {
            // aux variables
            int val;
            Set<String> newFeatures = new HashSet<>();
            Set<String> newParents = new HashSet<>();

            if (absmodel.debug)
                absmodel.println("Setting variables:");
            for (IntVar var : cpmodel.retrieveIntVars(true)) { // for all variables in the constraints
                // (absmodel): round 1
                // IF used variable is present in the configuration, update it!
                if (configuration.containsKey(var.getName())) {
                    val = configuration.get(var.getName());
                    if (absmodel.debug)
                        absmodel.println("  " + var + " (explicit) -> " + val);
                    var.eq(val).post();
                    // Possible feature name -- include later the parents.
                    if (val == 1) // TODO: check if variable is a feature
                        newFeatures.add(var.getName());
                }
            }
            // add parents of features from the configuration that are not in the
            // constraints (absmodel)
            for (Map.Entry<String, Integer> entry : configuration.entrySet()) {
                if (entry.getValue() == 1)
                    if (!entry.getKey().contains("."))
                        newFeatures.add(entry.getKey());
            }

            // collect parents of 'newFeatures'
            absmodel.collectParents(newFeatures, newParents);
            // add newParents and default values to the configuration
            for (IntVar var : cpmodel.retrieveIntVars(true)) {
                // (absmodel): round 2

                // If it is a parent to include, set
                if (newParents.contains(var.getName())) {
                    if (absmodel.debug)
                        absmodel.println("  " + var + " (parent) -> 1");
                    var.eq(1).post();
                }
                // ELSE use default value
                else if (!configuration.containsKey(var.getName())) {
                    if (defaultvals.containsKey(var.getName())) {
                        // By default, unrefered features & attributes are false
                        int defval = defaultvals.get(var.getName());
                        if (absmodel.debug)
                            absmodel.println("  " + var.getName() + " (default) -> " + defval);
                        var.eq(defval).post();
                    } else {
                        // Unknown variable, possibly created by
                        // ChocoSolver to model intermediate
                        // constraints -- ignore.  Can also be
                        // constant variable e.g. for top-level group
                        // names.
                    }
                }
            }
        }
        // Catch-all
        catch (Exception e1) {
            // Catch-all
            if (absmodel.debug) {
                System.err.println("$$$ Failed to check configuration... $$$");
                e1.printStackTrace();
            }
        }
        Solver solver = cpmodel.getSolver();
        Solution solution = solver.findSolution();
        if (absmodel.debug) {
            absmodel.println("  Solution: " + solution);
        }
        return solution;
    }

}
