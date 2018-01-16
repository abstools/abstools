/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.tests;

import java.io.PrintStream;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import abs.frontend.ast.ClassDecl;
import abs.frontend.ast.Decl;
import abs.frontend.ast.InterfaceDecl;
import abs.frontend.ast.MethodSig;
import abs.frontend.ast.Model;
import abs.frontend.ast.ModuleDecl;
import abs.frontend.ast.ParamDecl;
import abs.frontend.ast.ParametricDataTypeUse;
import abs.frontend.typechecker.Type;

/**
 *
 * @author pwong
 * @deprecated use {@link ASTBasedABSTestRunnerGenerator}
 */
@Deprecated
public class StringBasedABSTestRunnerGenerator extends AbstractABSTestRunnerGenerator {

    public StringBasedABSTestRunnerGenerator(Model model) {
        super(model);
    }

    private Map<InterfaceDecl, Set<ClassDecl>> tests = new HashMap<>();

    @Override
    public void generateTestRunner(PrintStream stream) {
        TestRunnerScriptBuilder imports = generateImports();
        TestRunnerScriptBuilder main = generateMainBlock(imports);
        stream.println("module "+RUNNER_MAIN+";");
        stream.println(imports.toString());
        stream.print("{");
        stream.print(TestRunnerScriptBuilder.NEWLINE);
        stream.print(TestRunnerScriptBuilder.INDENT);
        stream.print(main);
        stream.print(TestRunnerScriptBuilder.NEWLINE);
        stream.print("}");
        stream.print(TestRunnerScriptBuilder.NEWLINE);
    }

    /**
     * For each test interface and classes, append the corresponding
     * import declaration to a {@link StringBuilder} and return that
     * string builder
     *
     * @return a reference to that string builder
     */
    private TestRunnerScriptBuilder generateImports() {
        TestRunnerScriptBuilder builder = new TestRunnerScriptBuilder();
        for (InterfaceDecl key : tests.keySet()) {
            builder = generateImport(builder, key);
            for (ClassDecl clazz : tests.get(key)) {
                builder = generateImport(builder, clazz);
            }
        }
        return builder;
    }

    /**
     * Append a line of the form "import n from m;" to builder where n is the
     * name of the specified decl and m is the name of the {@link ModuleDecl} of
     * that decl.
     *
     * @param builder
     * @param decl
     * @return a reference to {@code builder}
     */
    private TestRunnerScriptBuilder generateImport(TestRunnerScriptBuilder builder, Decl decl) {
        return generateImport(builder, decl.getName(), decl.getModuleDecl().getName());
    }

    private TestRunnerScriptBuilder generateImport(TestRunnerScriptBuilder builder, String name, String module) {
        if (module.equals(absStdLib)) {
            return builder;
        }
        return builder.append("import ").append(name).append(" from ").append(module).append(";").newLine();
    }

    private TestRunnerScriptBuilder generateImports(TestRunnerScriptBuilder builder, Set<Type> types) {
        for (Type type : types) {
            generateImport(builder, type.getSimpleName(), type.getModuleName());
        }
        return builder;
    }

    private TestRunnerScriptBuilder generateMainBlock(TestRunnerScriptBuilder imports) {
        TestRunnerScriptBuilder builder = new TestRunnerScriptBuilder();
        builder.increaseIndent();
        Set<Type> paramNames = new HashSet<>();
        builder.append("Set<Fut<Unit>> ").append(futs).append(" = EmptySet;").newLine();
        builder.append("Fut<Unit> ").append(fut).append(";").newLine();
        for (InterfaceDecl key : tests.keySet()) {
            builder.append("//Test cases for ").append(key.getType().getQualifiedName()).newLine();
            for (ClassDecl clazz : tests.get(key)) {
                builder.append("//Test cases for implementation ").append(clazz.getName()).newLine();
                paramNames.addAll(generateTestClassImpl(key, clazz, builder));
            }
        }
        generateWaitSync(builder);
        generateImports(imports, paramNames);
        return builder;
    }

    private Set<Type> generateTestClassImpl(InterfaceDecl inf, ClassDecl clazz, TestRunnerScriptBuilder main) {
        Set<Type> paramNames = new HashSet<>();
        Type dataType = generateDataPoints(inf, clazz, paramNames, main);
        String namePrefix = clazz.getName();
        int instance = 0;
        for (MethodSig method : getTestMethods(inf)) {
            boolean needdata = method.getParamList().iterator().hasNext();
            if (needdata) {
                if (dataType == null) {
                    throw new IllegalStateException("Test method requires arguments but test class defines no data point");
                }
                /*
                 * a while loop over all data points
                 */
                String dataPointSet = dataPointSetName(clazz);
                main.append("while (hasNext(").append(dataPointSet).append(")) {").newLine().increaseIndent(); // begin while
                main.append("Pair<Set<").append(dataType).append(">,").append(dataType)
                    .append("> nt = next(").append(dataPointSet).append(");").newLine();
                main.append(dataType).append(" ").append(dataValue).append(" = snd(nt);").newLine();
                main.append(dataPointSet).append(" = fst(nt);").newLine();
            }

            /*
             * Add those methods that are not ignored
             */
            if (! isIgnored(clazz,method)) {
                main.append("//Test cases for method ").append(method.getName()).newLine();
                String objectRef = uncap(namePrefix) + instance;
                main = newCog(main, inf, clazz, objectRef);
                generateAsyncTestCall(main, objectRef, method);
            }

            if (needdata) {
                main.decreaseIndent().append("}").newLine(); // end while
            }
            instance++;
        }

        return paramNames;
    }

    /**
     * Generates data points for test class {@code clazz}
     *
     * @param inf
     * @param clazz
     * @param paramNames
     * @param main
     * @return The data type the set return type is parametric on, null if
     *         {@code inf} does not define a data point method or {@code clazz}
     *         does not implement such method.
     */
    private Type generateDataPoints(InterfaceDecl inf, ClassDecl clazz, Set<Type> paramNames, TestRunnerScriptBuilder main) {

        MethodSig dataPoint = findDataPoints(inf);
        if (dataPoint == null) {
            return null;
        }

        /*
         * It must be a Set of data
         */
        Type data = ((ParametricDataTypeUse) dataPoint.getReturnType()).getParams().iterator().next().getType();

        /*
         * make sure the return type can be resolved TODO this needs to be
         * resolved recursively
         */
        paramNames.add(data);

        /*
         * create an object in the same cog as main for retrieving the data
         * points
         */
        String objName = uncap(clazz.getName()) + "dataPoint";
        String dataSet = dataPointSetName(clazz);
        newObj(main, inf, clazz, objName, false);

        main.append(dataPoint.getReturnType()).append(" ").append(dataSet).append(" = ").append(objName).append(".")
                .append(dataPoint.getName()).append("();").newLine();

        return data;
    }

    private TestRunnerScriptBuilder generateWaitSync(TestRunnerScriptBuilder builder) {
        return
          builder
            .append("//waits for methods return...").newLine()
            .append("while (hasNext(").append(futs).append(")) {").newLine().increaseIndent() // begin while
            .append("Pair<Set<Fut<Unit>>,Fut<Unit>> nt = next(").append(futs).append(");").newLine()
            .append(fut).append(" = snd(nt);").newLine()
            .append(futs).append(" = fst(nt);").newLine()
            .append(fut).append(".get;").newLine()
            .decreaseIndent().append("}").newLine(); // end while
    }

    /**
     * Write the line of the form {@code objectRef!method(d);}, where d is
     * {@link #dataValue} to {@code builder}, if {@code method} takes an
     * argument, otherwise it write the line of the form
     * {@code objectRef!method();} to {@code builder}.
     *
     * @param main
     * @param objectRef
     * @param method
     */
    private void generateAsyncTestCall(TestRunnerScriptBuilder main, String objectRef, MethodSig method) {
        main.append(fut).append(" = ").append(objectRef).append("!").append(method.getName());
        Iterator<ParamDecl> paramIts = method.getParamList().iterator();
        if (paramIts.hasNext()) {
            main.append("(").append(dataValue).append(");").newLine(); // add
                                                                  // parameter
                                                                  // values
        } else {
            main.append("();").newLine(); // no parameter
        }
        main.append(futs).append("= Insert(").append(fut).append(",").append(futs).append(");").newLine();
    }

    /**
     * Write the line of the form {@code inf name = new cog clazz();} to
     * {@code builder}
     *
     * @param main
     * @param inf
     * @param clazz
     * @param name
     * @return a reference to builder.
     */
    private TestRunnerScriptBuilder newCog(TestRunnerScriptBuilder main, InterfaceDecl inf, ClassDecl clazz, String name) {
        return newObj(main, inf, clazz, name, true);
    }

    /**
     * If {@code cog} is true, this method writes the line of the form
     * {@code inf name = new cog clazz();} to {@code builder}, otherwise it
     * writes the line of the form {@code inf name = new clazz();} to
     * {@code builder}.
     *
     * @param main
     * @param inf
     * @param clazz
     * @param name
     * @param cog
     * @return a reference to builder.
     */
    private TestRunnerScriptBuilder newObj(TestRunnerScriptBuilder main, InterfaceDecl inf, ClassDecl clazz, String name, boolean cog) {
        return main.append(inf.getName()).append(" ").append(name).append((cog) ? " = new cog " : " = new ")
                .append(clazz.getName()).append("();").newLine();
    }
}
