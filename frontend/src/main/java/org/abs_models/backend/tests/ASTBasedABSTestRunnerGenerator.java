/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.tests;

import static org.abs_models.backend.tests.AbsASTBuilderUtil.getExpStmt;
import static org.abs_models.backend.tests.AbsASTBuilderUtil.getFnApp;
import static org.abs_models.backend.tests.AbsASTBuilderUtil.getFutUnitType;
import static org.abs_models.backend.tests.AbsASTBuilderUtil.getType;
import static org.abs_models.backend.tests.AbsASTBuilderUtil.getVAssign;
import static org.abs_models.backend.tests.AbsASTBuilderUtil.getVarDecl;
import static org.abs_models.backend.tests.AbsASTBuilderUtil.newObj;

import java.io.PrintStream;
import java.io.PrintWriter;
import java.util.HashSet;
import java.util.Set;

import org.abs_models.backend.prettyprint.ABSFormatter;
import org.abs_models.backend.prettyprint.DefaultABSFormatter;
import org.abs_models.frontend.ast.Access;
import org.abs_models.frontend.ast.AsyncCall;
import org.abs_models.frontend.ast.Block;
import org.abs_models.frontend.ast.ClassDecl;
import org.abs_models.frontend.ast.CompilationUnit;
import org.abs_models.frontend.ast.DataConstructorExp;
import org.abs_models.frontend.ast.DataTypeUse;
import org.abs_models.frontend.ast.FromImport;
import org.abs_models.frontend.ast.GetExp;
import org.abs_models.frontend.ast.Import;
import org.abs_models.frontend.ast.InterfaceDecl;
import org.abs_models.frontend.ast.List;
import org.abs_models.frontend.ast.MainBlock;
import org.abs_models.frontend.ast.MethodSig;
import org.abs_models.frontend.ast.Model;
import org.abs_models.frontend.ast.ModuleDecl;
import org.abs_models.frontend.ast.Name;
import org.abs_models.frontend.ast.NamedImport;
import org.abs_models.frontend.ast.ParametricDataTypeUse;
import org.abs_models.frontend.ast.PureExp;
import org.abs_models.frontend.ast.StarImport;
import org.abs_models.frontend.ast.SyncCall;
import org.abs_models.frontend.ast.TypeUse;
import org.abs_models.frontend.ast.VarDeclStmt;
import org.abs_models.frontend.ast.VarUse;
import org.abs_models.frontend.ast.WhileStmt;

/**
 * The ABSUnit test runner generator
 *
 * @author pwong
 *
 */
public class ASTBasedABSTestRunnerGenerator extends AbstractABSTestRunnerGenerator {


    public ASTBasedABSTestRunnerGenerator(Model model) {
        super(model);
    }

    @Override
    public void generateTestRunner(PrintStream stream) {
        // In order to safely call module.doPrettyPrint() we need a complete AST
        Model model = new Model();
        CompilationUnit compilationunit = new CompilationUnit();
        ModuleDecl module = new ModuleDecl();
        model.addCompilationUnitNoTransform(compilationunit);
        compilationunit.addModuleDeclNoTransform(module);
        module.setName(RUNNER_MAIN);
        module.setImportList(generateImportsAST());
        module.setBlock(generateMainBlockAST(module.getImportList()));

        PrintWriter writer = new PrintWriter(stream, true);
        ABSFormatter formatter = new DefaultABSFormatter(writer);
        module.doPrettyPrint(writer, formatter);
    }

    private void getImportsFrom(Set<String> mn, Set<String> qn, ModuleDecl m) {
        mn.add(m.getName());
        for (Import i : m.getImportList()) {
            if (i instanceof NamedImport) {
                for (Name n : ((NamedImport) i).getNameList()) {
                    qn.add(n.getName());
                }
            } else if (i instanceof FromImport) {
                mn.add(((FromImport) i).getModuleName());
            } else if (i instanceof StarImport) {
                mn.add(((StarImport) i).getModuleName());
            }
        }
    }

    private List<Import> generateImportsAST() {
        List<Import> imports = new List<>();
        Set<String> mn = new HashSet<>();
        Set<String> qn = new HashSet<>();
        for (InterfaceDecl key : tests.keySet()) {
            getImportsFrom(mn, qn, key.getModuleDecl());
            for (ClassDecl clazz : tests.get(key)) {
                getImportsFrom(mn, qn, clazz.getModuleDecl());
            }
        }

        for (String m : mn) {
            imports.add(new StarImport(m));
        }

        if (!qn.isEmpty()) {
            List<Name> names = new List<>();
            for (String q : qn) {
                names.add(new Name(q));
            }
            imports.add(new NamedImport(names));
        }

        return imports;
    }

    private MainBlock generateMainBlockAST(List<Import> list) {
        final MainBlock block = new MainBlock();

        DataConstructorExp empty = new DataConstructorExp("EmptySet", new List<>());
        VarDeclStmt futsStatement = getVarDecl(futs, getType("Set", getFutUnitType()), empty);
        block.addStmtNoTransform(futsStatement);

        VarDeclStmt futStatement = getVarDecl(fut, getFutUnitType(), null);
        block.addStmtNoTransform(futStatement);

        Set<TypeUse> use = new HashSet<>();
        for (InterfaceDecl key : tests.keySet()) {
            for (ClassDecl clazz : tests.get(key)) {
                use.addAll(generateTestClassImplAST(key, clazz, block));
            }
        }

        block.addStmtNoTransform(generateWaitSyncAST());

        return block;
    }

    private WhileStmt generateWaitSyncAST() {
        WhileStmt ws = new WhileStmt();
        ws.setCondition(getFnApp("hasNext",new VarUse(futs)));
        Block body = new Block();
        DataTypeUse u = getType("Pair", getType("Set",
                getType("Fut", getType("Unit"))),getType("Fut", getType("Unit")));
        body.addStmtNoTransform(getVarDecl("nt", u, getFnApp("next",new VarUse(futs))));
        body.addStmtNoTransform(getVAssign(fut, getFnApp("snd",new VarUse("nt"))));
        body.addStmtNoTransform(getVAssign(futs, getFnApp("fst",new VarUse("nt"))));
        body.addStmtNoTransform(getExpStmt(new GetExp(new VarUse("fut"))));
        // Attach body at the end, since JastAdd will avoid touching ASTs without parents.
        ws.setBody(body);
        return ws;
    }

    private Set<TypeUse> generateTestClassImplAST(
            InterfaceDecl inf, ClassDecl clazz, MainBlock block) {
        Set<TypeUse> accesses = new HashSet<>();
        TypeUse dataType = generateDataPointsAST(inf, clazz, accesses, block);

        String namePrefix = clazz.getName();
        int instance = 0;
        for (MethodSig method : getTestMethods(inf)) {
            Block thisBlock = block;
            WhileStmt ws = null;

            if (method.getNumParam() > 0) {
                if (dataType == null) {
                    throw new IllegalStateException("Test method requires arguments but test class defines no data point");
                }
                /*
                 * a while loop over all data points
                 */
                String dataPointSet = dataPointSetName(clazz);
                ws = new WhileStmt();
                ws.setCondition(getFnApp("hasNext",new VarUse(dataPointSet)));
                Block body = new Block();
                thisBlock = body;
                DataTypeUse u = getType("Pair", getType("Set", (TypeUse)dataType.copy()), (TypeUse)dataType.copy());
                thisBlock.addStmtNoTransform(getVarDecl("nt", u, getFnApp("next",new VarUse(dataPointSet))));
                thisBlock.addStmtNoTransform(getVarDecl(dataValue, (TypeUse)dataType.copy(), getFnApp("snd",new VarUse("nt"))));
                thisBlock.addStmtNoTransform(getVAssign(dataPointSet, getFnApp("fst",new VarUse("nt"))));
                ws.setBody(body);
            }

            /*
             * Add those methods that are not ignored
             */
            if (! isIgnored(clazz,method)) {
                String objectRef = uncap(namePrefix) + instance;
                thisBlock.addStmtNoTransform(newObj(inf, clazz, objectRef, false));
                generateAsyncTestCallAST(thisBlock, objectRef, method);
            }

            if (ws != null) {
               block.addStmtNoTransform(ws);
            }
            instance++;
        }

        return accesses;
    }

    private TypeUse generateDataPointsAST(InterfaceDecl key, ClassDecl clazz,
            Set<TypeUse> use, MainBlock block) {
        MethodSig dataPoint = findDataPoints(key);
        if (dataPoint == null) {
            return null;
        }

        Access rt = dataPoint.getReturnType();
        if (!(rt instanceof ParametricDataTypeUse)) {
            return null;
        }

        ParametricDataTypeUse prt = (ParametricDataTypeUse) rt;
        if (! prt.getName().equals("Set")) {
            return null;
        }

        //Set has only one type parameter
        TypeUse u = (TypeUse) prt.getParam(0).copy();
        use.add(u);

        String objName = uncap(clazz.getName()) + "dataPoint";
        String dataSet = dataPointSetName(clazz);
        block.addStmtNoTransform(newObj(key, clazz, objName, true));
        block.addStmtNoTransform(getVarDecl(dataSet, prt.copy(),
             new SyncCall(new VarUse(objName), dataPoint.getName(), new List<>())));

        return u;
    }


    private void generateAsyncTestCallAST(Block block, String objectRef, MethodSig method) {
        List<PureExp> args = new List<>();
        if (method.getNumParam() > 0) {
            args.add(new VarUse(dataValue));
        }
        block.addStmtNoTransform(getVAssign(fut, new AsyncCall(new VarUse(objectRef), method.getName(), args)));
        block.addStmtNoTransform(getVAssign(futs, getFnApp("Insert", new VarUse(fut), new VarUse(futs))));
    }


}
