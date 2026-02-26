package org.abs_models.frontend.typechecker;

import org.abs_models.Absc;
import org.abs_models.common.WrongProgramArgumentException;
import org.abs_models.frontend.analyser.SemanticCondition;
import org.abs_models.frontend.analyser.SemanticConditionList;
import org.abs_models.frontend.ast.AttrAssignment;
import org.abs_models.frontend.ast.Feature;
import org.abs_models.frontend.ast.IntVal;
import org.abs_models.frontend.ast.Model;
import org.abs_models.frontend.ast.Product;
import org.abs_models.frontend.ast.ProductDecl;
import org.abs_models.frontend.delta.ProductLineAnalysisHelper;
import org.abs_models.frontend.mtvl.ChocoSolver;
import org.abs_models.frontend.parser.Main;
import picocli.CommandLine.Command;
import picocli.CommandLine.Option;
import picocli.CommandLine.Parameters;
import picocli.CommandLine.ParentCommand;

import java.io.File;
import java.util.*;
import java.util.concurrent.Callable;

@Command(name = "checkspl",
         description = "Perform software product line checking for abs models",
         abbreviateSynopsis = true,
         sortOptions = false,   // print options in the same order as they are defined
         helpCommand = true,
         mixinStandardHelpOptions = true // handles -h, -V
)
public class CheckSPLCommand implements Callable<Void> {
    @ParentCommand
    private Absc parent;

    @Parameters(description = "All abs files/directories/packages that comprise the productline",
                arity = "1..*")
    public List<File> files;

    // mTVL options
    @Option(names = { "--solve" },
            description = "solve constraint satisfaction problem (CSP) for the feature model and print all solutions")
    public boolean solve = false ;
    @Option(names = { "--solve-all" },
            description = "solve all solutions for the CSP and print timing information")
    public boolean solveall = false ;
    @Option(names = { "--solve-with" },
            description = "solve CSP by finding all feature configurations for product @|italic PID|@",
            paramLabel = "product")
    public String solveWithProduct ;
    @Option(names = { "--min" },
            description = "minimise variable @|italic var|@ when solving the CSP for the feature model",
            paramLabel = "var")
    public String minimise;
    @Option(names = { "--max" },
            description = "maximise variable @|italic var|@ when solving the CSP for the feature model",
            paramLabel = "var")
    public String maximise;
    @Option(names = { "--max-product" },
            description = "print the solution that has the most number of features")
    public boolean maxProduct = false ;
    @Option(names = { "--min-with" },
            description = "solve CSP by finding a solution that tries to include @|italic product|@ with minimum number of changes",
            paramLabel = "product")
    public String minWith;
    @Option(names = { "--nsol" },
            description = "count the number of solutions")
    public boolean numbersol = false ;
    @Option(names = { "--noattr" },
            description = "ignore attributes when generating product")
    public boolean ignoreattr = false ;
    @Option(names = { "--check" },
            description = "check satisfiability of @|italic product|@",
            paramLabel = "product")
    public String checkProduct;

    private void typeCheckProductLine(Model m) {

        int n = m.getProductList().getNumChild();
        if (n == 0)
            return;

        if (parent.verbose) {
            System.out.println("Typechecking Software Product Line (" + n + " products)...");
        }
        SemanticConditionList errors = m.typeCheckPL();
        for (SemanticCondition err : errors) {
            System.err.println(err.getHelpMessage());
        }
    }

    private void analyzeMTVL(Model m) {
        if (m.hasMTVL()) {
            if (solve) {
                if (parent.verbose)
                    System.out.println("Searching for solutions for the feature model...");
                ChocoSolver s = ChocoSolver.fromModel(m);
                System.out.print(s.getSolutionsAsString());
            }
            if (minimise != null) {
                if (parent.verbose)
                    System.out.println("Searching for minimum solutions of "+minimise+" for the feature model...");
                ChocoSolver s = ChocoSolver.fromModel(m);
                System.out.print(s.minimiseToString(minimise));
            }
            if (maximise != null) {
                if (parent.verbose)
                    System.out.println("Searching for maximum solutions of "+maximise+" for the feature model...");
                // (rudi 2026-02-24): the following three lines of
                // code seem to be dead; removing the last instance of
                // external addConstraint.  If creating the constraint
                // is necessary, they should be made into a method of
                // the ChocoSolver class.

                // ChocoSolver s = ChocoSolver.fromModel(m);
                // //System.out.print(s.maximiseToInt(product));
                // s.addConstraint(ChocoSolver.eqeq(s.getVars().get(maximise), s.maximiseToInt(maximise)));
                ChocoSolver s1 = ChocoSolver.fromModel(m);
                int i=1;
                for (Map<String, Integer> solution : s1.getSolutions()) {
                    System.out.println("------ "+(i++)+"------");
                    System.out.print(solution);
                }
            }
            if (solveWithProduct != null) {
                ProductDecl solveWithDecl = null;
                try {
                    solveWithDecl = m.findProduct(solveWithProduct);
                } catch (WrongProgramArgumentException e) {
                    // nothing to do
                }
                if (solveWithDecl != null) {
                    if (parent.verbose)
                        System.out.println("Searching for solution that includes " + solveWithProduct + "...");
                    ChocoSolver s = ChocoSolver.fromModel(m);
                    s.addProductConstraints(solveWithDecl.getProduct());
                    System.out.println("checking solution:\n" + s.getSolutionsAsString());
                } else {
                    System.out.println("Product '" + solveWithProduct + "' not found.");
                }
            }
            if (minWith != null) {
                ProductDecl minWithDecl = null;
                try {
                    minWithDecl = m.findProduct(minWith);
                } catch (WrongProgramArgumentException e) {
                    // nothing to do
                }
                if (minWithDecl != null) {
                    if (parent.verbose)
                        System.out.println("Searching for solution that includes " + minWith + "...");
                    System.out.println("checking solution: " + ChocoSolver.calculateMinFeaturesOfProduct(m, minWithDecl.getProduct()));
                } else {
                    System.out.println("Product '" + minWith + "' not found.");
                }

            }
            if (maxProduct) {
                if (parent.verbose)
                    System.out.println("Searching for solution with maximum number of features ...");
                System.out.println("checking solution: "+ChocoSolver.calculateMaxProductFeatures(m));
            }
            if (checkProduct != null) {

                ProductDecl checkProductDecl = null;
                try {
                    checkProductDecl = m.findProduct(checkProduct);
                } catch (WrongProgramArgumentException e) {
                    // nothing to do
                }
                if (checkProductDecl == null ){
                    System.out.println("Product '" + checkProduct + "' not found, cannot check.");
                } else {
                    List<String> errors = ChocoSolver.checkProduct(checkProductDecl.getProduct(), m);
                    System.out.println("checking solution...");
                    for (String error : errors)
                        System.out.println("Constraint failed: " + error);
                    if (errors.isEmpty()) System.out.println("No constraints failed.");
                }
            }
            if (numbersol) {
                ChocoSolver s = ChocoSolver.fromModel(m);
                // did we call m.dropAttributes() previously?
                if (ignoreattr) {
                    System.out.println("Number of solutions found (without attributes): "+s.countSolutions());
                } else {
                    System.out.println("Number of solutions found: "+s.countSolutions());
                }
            }
        }
    }


    void analyzeModel(Model m) {
        m.verbose = parent.verbose;
        m.debug = parent.debug;

        // drop attributes before calculating any attribute
        if (ignoreattr)
            m.dropAttributes();

        if (parent.verbose) {
            System.out.println("Analyzing Software Product Line...");
        }

        // check PL before flattening
        typeCheckProductLine(m);

        // flatten before checking error, to avoid calculating *wrong* attributes
        if (solveall) {
            // Build all SPL configurations (valid feature selections, ignoring attributes), one by one (for performance measuring)
            if (parent.verbose)
                System.out.println("Building ALL " + m.getProductList().getNumChild() + " feature model configurations...");
            buildAndPrintAllConfigurations(m);
        }
        // TODO: check if there were errors
        analyzeMTVL(m);
    }

    /*
     * Build all SPL configurations (valid feature selections, ignoring attributes), one by one
     * The purpose is to measure how long this takes, so we can compare it with the performance of type checking the SPL.
     *
     */
    private static void buildAndPrintAllConfigurations(Model m) {

        long timeSum = 0;
        for (Product product : m.getProductList()) {

            long time0 = System.currentTimeMillis();
            System.out.println("\u23F1 Flattening product: " + product.getFeatureSetAsString());

            // Find a solution to the feature model that satisfies the product feature selection
            ChocoSolver s = ChocoSolver.fromModel(m);
            s.addProductConstraints(product);

            Map<String, Integer> solution = s.getSolution();
            System.out.println("\u23F1 Full product configuration: " + solution);
            long time1 = System.currentTimeMillis();

            // map the solution to the product,
            // i.e. add attribute assignments to features
            for (String fname : solution.keySet()) {
                if (fname.startsWith("$")) // ignore internal ChocoSolver variable
                    continue;
                if (fname.contains(".")) {
                    String[] parts = fname.split("\\.");
                    String fid = parts[0];
                    String aid = parts[1];
                    Integer val = solution.get(fname);
                    for (Feature feature : product.getFeatures()) {
                        if (feature.getName().equals(fid)) {
                            feature.addAttrAssignment(new AttrAssignment(aid, new IntVal(val)));
                            break;
                        }
                    }
                }
            }

            long time2 = System.currentTimeMillis();

            Model thisModel = m.treeCopyNoTransform();

            long time3 = System.currentTimeMillis();
            if (thisModel.getProductLine() != null) thisModel.flattenForProduct(product);

            long time4 = System.currentTimeMillis();
            timeSum += (time4 - time3);
            System.out.println("\u23F1 Time: " + (time1 - time0) + " | " + (time2 - time1) + " | " + (time3 - time2) + " | " + (time4 - time3) + " | " + "Total(s): " + ((time4 - time0)/1000.0));
        }
        System.out.println("\u23F1 Flattening total time (s): " + timeSum/1000.0);
        if (m.getProductLine() == null) System.out.println("Note: model has no productline definition, so no flattening performed.");
    }

    @Override
    public Void call() throws Exception {
        if (parent.verbose) System.out.println("Starting software product line checking ...");
        Main main = new Main();
        main.arguments = this.parent; // FIXME: fill in parent's verbose, debug
        Model m = main.parse(files);
        analyzeModel(m);
        return null;
    }
}
