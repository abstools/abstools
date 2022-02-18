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
import java.util.HashSet;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.stream.Collectors;
import java.util.stream.Stream;

@CommandLine.Command(name = "checkvar-nosat",
    description = "Perform software product line static checking for the variability modules",
    abbreviateSynopsis = true,
    sortOptions = false,
    helpCommand = true,
    mixinStandardHelpOptions = true
)
public class NoSatCheckVarCommand implements Callable<Void> {
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
                for(Name name : ((NamedExport) export).getNames()) {
                    for(LocalOpenProductDecl opl : module.getLocalOpenProductDeclList())
                        if(opl.getName().equals(name))
                            e.add(new SemanticError(module, ErrorMessage.EXPORTED_OPEN, ""));
                    exported.add(name.getName());
                }
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

    public void execute(Model m){

        long startPre = System.currentTimeMillis();
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
            
            sanityProductConfigurations(module, e);

            //SC5 + SC7 Exports correct
            exportCheck(module, e);

            //SC3 Imports correct is reused from standard type checker
            for(Import i : module.getImports()){
                i.typeCheck(e);
            }

        }

        //SC1 and SC4 are part of the standard checker for imports and exports


        // Compute signatures + check type uniformity on the fly
        ModelFamilySignature signature = new ModelFamilySignature(m, e);


        // Acyclic inheritance
        signature.checkInheritance(e);

        // pre typing
        m.varTypeCheck(e, signature, false);

        long endPre = System.currentTimeMillis();
        long start = System.currentTimeMillis();
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
        long end = System.currentTimeMillis();

        System.out.println("Finished");
        for(SemanticCondition x : e) {
            System.out.println(x);
        }
        System.out.println("checks: "+(endPre-startPre));
        System.out.println("app: "+(end-start));
        System.out.println("size: "+c.pretty());
    }

    public HashSet<Feature> getFeats(ProductExpr expr){
        HashSet<Feature> res = new HashSet<>();
        if(expr instanceof ProductFeatureSet){
            for(Feature f : ((ProductFeatureSet) expr).getFeatures())
                res.add(f);
        } else if(expr instanceof ProductName) {
            //skip
        } else if(expr instanceof ProductDifference) {
            res.addAll(getFeats(((ProductDifference) expr).getLeft()));
            res.addAll(getFeats(((ProductDifference) expr).getRight()));
        } else if(expr instanceof ProductUnion) {
            res.addAll(getFeats(((ProductUnion) expr).getLeft()));
            res.addAll(getFeats(((ProductUnion) expr).getRight()));
        } else if(expr instanceof ProductIntersect) {
            res.addAll(getFeats(((ProductIntersect) expr).getLeft()));
            res.addAll(getFeats(((ProductIntersect) expr).getRight()));
        }
        return res;
    }

    private void sanityProductConfigurations(ModuleDecl module, SemanticConditionList e) {
        HashSet<Feature> feats = new HashSet<>();
        for(Feature f : module.getProductLine().getFeatures())
            feats.add(f);
        for(LocalProductDecl decl : module.getLocalProductDeclList()) {
            HashSet<Feature> inside = getFeats(decl.getProductExpr());
            //SC 8-a-ii
            if(!decl.getProductExpr().noNamesFrom(null)) e.add(new SemanticError(module, ErrorMessage.OTHER,""));
            //SC 8-a-i and 8-b-1
            if(!feats.containsAll(inside)) e.add(new SemanticError(module, ErrorMessage.OTHER,""));
        }
        for(LocalOpenProductDecl open : module.getLocalOpenProductDeclList()){
            for(LocalProductBranch branch : open.getLocalProductBranchs()){
                ProductExpr body = branch.getbody();
                // 8-e-i
                if(!body.onlyNamesFrom(module)) e.add(new SemanticError(module, ErrorMessage.OTHER,""));
                HashSet<Feature> inside = getFeats(body);
                Stream<ModuleDecl> a = inside.stream().map(ASTNode::getModuleDecl);
                // 8-e-others
                if(a.collect(Collectors.toSet()).size() != 1) e.add(new SemanticError(module, ErrorMessage.OTHER,""));
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

        this.execute(m);

        return null;
    }
}
