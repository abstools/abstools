package org.abs_models.frontend.variablechecker;

import choco.kernel.model.constraints.Constraint;
import org.abs_models.Absc;
import org.abs_models.frontend.analyser.*;
import org.abs_models.frontend.ast.*;
import org.abs_models.frontend.mtvl.ChocoSolver;
import org.abs_models.frontend.parser.Main;
import org.abs_models.frontend.typechecker.ResolvedName;
import picocli.CommandLine;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Callable;

@CommandLine.Command(name = "checkvar",
    description = "Perform software product line static checking for the variability modules",
    abbreviateSynopsis = true,
    sortOptions = false,
    helpCommand = true,
    mixinStandardHelpOptions = true
)
public class CheckVarCommand implements Callable<Void> {
    @CommandLine.ParentCommand
    private Absc parent;

    @CommandLine.Parameters(description = "All abs files/directories/packages that comprise the productline",
        arity = "1..*")
    public List<File> files;

    @CommandLine.Option(names = { "-v", "--verbose" },
        description = "verbose output")
    public boolean verbose = false;
    @CommandLine.Option(names = { "--debug"},
        description = "print diagnostic information (e.g., stacktraces) for internal compiler problems")
    public boolean debug = false;

    public void exportCheck(ModuleDecl module, SemanticConditionList e){

        boolean exportAll = false;
        ArrayList<String> exported = new ArrayList<>();
        for(Export export : module.getExports()){
            if(export instanceof StarExport){
                exportAll = true;
                break;
            }
            if(export instanceof NamedExport){
                for(Name name : ((NamedExport) export).getNames()) exported.add(name.getName());
            }
            if(export instanceof FromExport){
                e.add(new SemanticError(module,ErrorMessage.EXPORTED_REFERENCE,""));
            }
        }
        if(!exportAll){
            ArrayList<String> declared = new ArrayList<>();
            for( ResolvedName name : module.getDefinedNames().values() )
                declared.add(name.getSimpleName().getName());
            for (DeltaDecl ddecl : module.getProductLine().getDeltaDecls()) {
                for (ModuleModifier moduleModifier : ddecl.getModuleModifiers()){
                    declared.add(moduleModifier.getName());
                }
            }
            exported.removeAll(declared);
            if(!exported.isEmpty()){
                e.add(new SemanticError(module,ErrorMessage.EXPORTED_UNKNOWN,""));
            }
        }
    }

    @Override
    public Void call() throws Exception {
        if (verbose) System.out.println("Starting static checks of variability modules ...");
        Main main = new Main();
        main.arguments = this.parent;
        Model m = main.parseFiles(main.arguments.verbose, files);
        m.evaluateAllProductDeclarations(); // resolve ProductExpressions to simple sets of features
        Main.rewriteModel(m, main.arguments.product);
        m.flattenTraitOnly();
        m.collapseTraitModifiers();

        m.expandPartialFunctions();
        m.expandForeachLoops();
        m.expandAwaitAsyncCalls();

        m.getProductDeclsFromExps();

        if (main.arguments.product != null) {
            if (main.arguments.notypecheck) {
                m.flattenForProductUnsafe(main.arguments.product);
            } else {
                m.flattenForProduct(main.arguments.product);
            }
        }

        SemanticConditionList e = new SemanticConditionList();

        //SC2: Only one main block
        int mainCount = 0;
        for(ModuleDecl decl : m.getModuleDecls()){
            if(decl.hasBlock()) mainCount++;
        }
        if(mainCount >= 2) e.add(new SemanticError(m, ErrorMessage.MAIN_BLOCK_MULTI,""));


        //Per-module SCs
        for(ModuleDecl module : m.getModuleDecls()){

            //SC6 Warnings for redundant UMs
            if(!module.hasProductLine()){
                for(Decl decl : module.getDecls()){
                    if(decl instanceof InterfaceDecl && ((InterfaceDecl) decl).getUnique())
                        e.add(new SemanticWarning(decl,ErrorMessage.UNIQUE_REDUNDANT,""));
                    if(decl instanceof ClassDecl && ((ClassDecl) decl).getUnique())
                        e.add(new SemanticWarning(decl,ErrorMessage.UNIQUE_REDUNDANT,""));

                }
                continue; //ignore UMs otherwise
            }


            //SC5 Exports correct
            exportCheck(module, e);

            //SC3 Imports correct is reused from standard type checker
            for(Import i : module.getImports()){
                i.typeCheck(e);
            }
        }


        //collect all VM references
        //m.getInterfaceTypeUseAndExpsWithProductDecl();




        // TODO: 29.12.2021 Sanity checks
        // TODO: (1) All VM names declared // This is not VM specific? //No dangling references to modules?
        // TODO: (4) Only imported and declared names are used // This is not VM specific?
        // TODO: (7) No open products are exported // They are not implemented yet anyway
        // TODO: (8) Proper definitions of products // Postpone until SAT solver is added
        // TODO: (9) Well-defined KEs // Postpone until SAT solver is added


        // Compute signatures + check type uniformity on the fly
        ModelFamilySignature signature = new ModelFamilySignature(m, e);


        // Acyclic inheritance
        signature.checkInheritance(e);

        // pre typing
        // TODO: redo
        m.varTypeCheck(e, signature);

        ApplicationConstraints constraints = m.applyVarCheck(e, signature);

        ChocoSolver solver = new ChocoSolver();
        for(ModuleDecl mDecl : m.getModuleDecls()) {
            if(mDecl.hasProductLine()) {
                for(Feature feat : mDecl.getProductLine().getFeatures())
                    solver.addBoolVar(feat.getName());
            }
        }

        Constraint c = constraints.getPsi().translateToChoco(solver);
        solver.addConstraint(c);
        if(!solver.solve())
            e.add(new SemanticError(m,ErrorMessage.APPLICABILITY_FAIL,""));

        System.out.println("Finished");
        for(SemanticCondition x : e) {
            System.out.println(x);
        }
        return null;
    }
}
