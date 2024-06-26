package org.abs_models.frontend.typechecker;

import java.io.File;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.concurrent.Callable;

import org.abs_models.Absc;
import org.abs_models.common.WrongProgramArgumentException;
import org.abs_models.frontend.analyser.SemanticCondition;
import org.abs_models.frontend.analyser.SemanticConditionList;
import org.abs_models.frontend.ast.Model;
import org.abs_models.frontend.ast.ProductDecl;
import org.abs_models.frontend.delta.ProductLineAnalysisHelper;
import org.abs_models.frontend.mtvl.ChocoSolver;
import org.abs_models.frontend.parser.Main;

import choco.kernel.model.constraints.Constraint;
import picocli.CommandLine.Command;
import picocli.CommandLine.Option;
import picocli.CommandLine.Parameters;
import picocli.CommandLine.ParentCommand;

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
            description = "solve constraint satisfaction problem (CSP) for the feature model and print a solution")
    public boolean solve = false ;
    @Option(names = { "--solve-all" },
            description = "solve all solutions for the CSP and print timing information")
    public boolean solveall = false ;
    @Option(names = { "--solve-with" },
            description = "solve CSP by finding a product that includes @|italic PID|@",
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

        //int n = m.getFeatureModelConfigurations().size();
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
                ChocoSolver s = m.instantiateCSModel();
                System.out.print(s.getSolutionsAsString());
            }
            if (minimise != null) {
                if (parent.verbose)
                    System.out.println("Searching for minimum solutions of "+minimise+" for the feature model...");
                ChocoSolver s = m.instantiateCSModel();
                System.out.print(s.minimiseToString(minimise));
            }
            if (maximise != null) {
                if (parent.verbose)
                    System.out.println("Searching for maximum solutions of "+maximise+" for the feature model...");
                ChocoSolver s = m.instantiateCSModel();
                //System.out.print(s.maximiseToInt(product));
                s.addConstraint(ChocoSolver.eqeq(s.getVars().get(maximise), s.maximiseToInt(maximise)));
                ChocoSolver s1 = m.instantiateCSModel();
                int i=1;
                while(s1.solveAgain()) {
                    System.out.println("------ "+(i++)+"------");
                    System.out.print(s1.getSolutionsAsString());
                }
            }
            if (solveall) {
                if (parent.verbose)
                    System.out.println("Searching for all solutions for the feature model...");
                ChocoSolver solver = m.instantiateCSModel();
                System.out.print(solver.getSolutionsAsString());
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
                    ChocoSolver s = m.instantiateCSModel();
                    HashSet<Constraint> newcs = new HashSet<>();
                    solveWithDecl.getProduct().getProdConstraints(s.getVars(), newcs);
                    for (Constraint c: newcs)
                        s.addConstraint(c);
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
                    ChocoSolver s = m.instantiateCSModel();
                    HashSet<Constraint> newcs = new HashSet<>();
                    s.addIntVar("difference", 0, 50);
                    m.getDiffConstraints(minWithDecl.getProduct(), s.getVars(), newcs, "difference");
                    for (Constraint c: newcs) s.addConstraint(c);
                    System.out.println("checking solution: " + s.minimiseToString("difference"));
                } else {
                    System.out.println("Product '" + minWith + "' not found.");
                }

            }
            if (maxProduct) {
                if (parent.verbose)
                    System.out.println("Searching for solution with maximum number of features ...");
                ChocoSolver s = m.instantiateCSModel();
                HashSet<Constraint> newcs = new HashSet<>();
                s.addIntVar("noOfFeatures", 0, 50);
                if (m.getMaxConstraints(s.getVars(),newcs, "noOfFeatures")) {
                    for (Constraint c: newcs) s.addConstraint(c);
                    System.out.println("checking solution: "+s.maximiseToString("noOfFeatures"));
                }
                else {
                    System.out.println("---No solution-------------");
                }

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
                    ChocoSolver s = m.instantiateCSModel();
                    Map<String,Integer> guess = checkProductDecl.getProduct().getSolution();
                    System.out.println("checking solution: "+s.checkSolution(guess,m));
                }
            }
            if (numbersol) {
                ChocoSolver s = m.instantiateCSModel();
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
            ProductLineAnalysisHelper.buildAndPrintAllConfigurations(m);
        }
        // TODO: check if there were errors
        analyzeMTVL(m);
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
