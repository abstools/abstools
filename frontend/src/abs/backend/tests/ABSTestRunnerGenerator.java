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

import abs.frontend.ast.Annotation;
import abs.frontend.ast.ClassDecl;
import abs.frontend.ast.DataConstructor;
import abs.frontend.ast.DataConstructorExp;
import abs.frontend.ast.Decl;
import abs.frontend.ast.InterfaceDecl;
import abs.frontend.ast.InterfaceTypeUse;
import abs.frontend.ast.List;
import abs.frontend.ast.MethodSig;
import abs.frontend.ast.Model;
import abs.frontend.ast.ModuleDecl;
import abs.frontend.ast.ParamDecl;
import abs.frontend.ast.ParametricDataTypeDecl;
import abs.frontend.ast.ParametricDataTypeUse;
import abs.frontend.ast.PureExp;
import abs.frontend.parser.Main;
import abs.frontend.typechecker.Type;

/**
 * The ABSUnit test runner generator
 * 
 * @author pwong
 * 
 */
public class ABSTestRunnerGenerator extends Main {
    
    public static final String RUNNER_MAIN = "AbsUnit.TestRunner";
    
    private static final String test = "AbsUnit.Test";
    private static final String dataPoint = "AbsUnit.DataPoint";
    private static final String testClass = "AbsUnit.TestClass";
    private static final String testClassImpl = "AbsUnit.TestClassImpl";
    private static final String absStdSet = "ABS.StdLib.Set";
    private static final String absStdLib = "ABS.StdLib";

    private static final String dataValue = "d";
    private static final String futs = "futs";
    private static final String fut = "fut";

    private DataConstructor testType;
    private DataConstructor dataPointType;
    private DataConstructor testClassType;
    private DataConstructor testClassImplType;

    private Map<InterfaceDecl, Set<ClassDecl>> tests = new HashMap<InterfaceDecl, Set<ClassDecl>>();

    private final Model model;

    private boolean isEmpty = true;

    /**
     * The constructor takes a type checked {@link Model} of the ABS model
     * 
     * @param model
     */
    public ABSTestRunnerGenerator(Model model) {
        assert model != null : "Model cannot be null!" ;
        
        this.model = model;

        gatherABSUnitAnnotations();

        /*
         * Do not search for test class definitions if this model does not
         * contain the necessary ABSUnit annotations
         */
        if (testType == null || dataPointType == null || testClassType == null || testClassImplType == null) {
            return;
        }

        gatherTestClasses();

        /*
         * An ABSUnit ABS model must have defined at least one interface with
         * TestClass annotation.
         */
        if (tests.isEmpty())
            return;

        /*
         * An ABSUnit ABS model must have defined at least one class with
         * TestClassImpl annotation.
         */
        for (InterfaceDecl inf : tests.keySet()) {
            isEmpty &= tests.get(inf).isEmpty();
        }
    }

    /**
     * Checks if this generator contains a {@link Model} that defines ABSUnit
     * tests.
     * 
     * @return
     */
    public boolean hasUnitTest() {
        return !isEmpty;
    }

    /**
     * Outputs an ABS module that only contains a main block. For each test
     * method defined in the model, this main blo annotated test classes
     * 
     * @param stream
     */
    public void generateTestRunner(PrintStream stream) {
        StringBuilder imports = generateImports();
        StringBuilder main = generateMainBlock(imports);
        stream.println("module "+RUNNER_MAIN+";");
        stream.println(imports.toString());
        stream.println("{");
        stream.print(main);
        stream.println("}");
    }

    private String uncap(String word) {
        return new StringBuilder().append(Character.toLowerCase(word.charAt(0))).append(word.substring(1)).toString();
    }

    private void gatherTestClasses() {
        for (Decl decl : this.model.getDecls()) {
            if (decl instanceof ClassDecl) {
                ClassDecl clazz = (ClassDecl) decl;
                if (isTestClassImpl(clazz)) {
                    InterfaceDecl inf = getTestClass(clazz);
                    if (inf != null) {
                        addTest(inf, clazz);
                    }
                    continue;
                }
            }
        }
    }

    private void addTest(InterfaceDecl inf, ClassDecl clazz) {
        if (!tests.containsKey(inf)) {
            tests.put(inf, new HashSet<ClassDecl>());
        }
        tests.get(inf).add(clazz);
    }

    private boolean hasTestAnnotation(List<Annotation> annotations, DataConstructor constructor) {
        for (Annotation ta : annotations) {
            PureExp exp = ta.getValue();
            if (exp instanceof DataConstructorExp
                    && ((DataConstructorExp) exp).getDataConstructor().equals(constructor)) {
                return true;
            }
        }
        return false;
    }

    private InterfaceDecl getTestClass(ClassDecl clazz) {
        for (InterfaceTypeUse inf : clazz.getImplementedInterfaceUseList()) {
            if (inf.getDecl() instanceof InterfaceDecl) {
                InterfaceDecl decl = (InterfaceDecl) inf.getDecl();
                if (hasTestAnnotation(decl.getAnnotations(), testClassType)) {
                    return decl;
                }
            }
        }
        return null;
    }

    private boolean isTestClassImpl(ClassDecl clazz) {
        return hasTestAnnotation(clazz.getAnnotations(), testClassImplType);
    }

    private void gatherABSUnitAnnotations() {
        for (Decl decl : this.model.getDecls()) {
            if (decl instanceof ParametricDataTypeDecl) {
                if (decl.getType().getQualifiedName().equals(test)) {
                    testType = ((ParametricDataTypeDecl) decl).getDataConstructor(0);
                } else if (decl.getType().getQualifiedName().equals(testClass)) {
                    testClassType = ((ParametricDataTypeDecl) decl).getDataConstructor(0);
                } else if (decl.getType().getQualifiedName().equals(testClassImpl)) {
                    testClassImplType = ((ParametricDataTypeDecl) decl).getDataConstructor(0);
                } else if (decl.getType().getQualifiedName().equals(dataPoint)) {
                    dataPointType = ((ParametricDataTypeDecl) decl).getDataConstructor(0);
                }
            }
        }
    }

    /**
     * For each test interface and classes, append the corresponding
     * import declaration to a {@link StringBuilder} and return that
     * string builder
     * 
     * @return a reference to that string builder
     */
    private StringBuilder generateImports() {
        StringBuilder builder = new StringBuilder();
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
    private StringBuilder generateImport(StringBuilder builder, Decl decl) {
        return generateImport(builder, decl.getName(), decl.getModuleDecl().getName());
    }

    private StringBuilder generateImports(StringBuilder builder, Set<Type> types) {
        for (Type type : types) {
            generateImport(builder, type.getSimpleName(), type.getModuleName());   
        }
        return builder;
    }

    private StringBuilder generateImport(StringBuilder builder, String name, String module) {
        if (module.equals(absStdLib)) {
            return builder;
        }
        return builder.append("import ").append(name).append(" from ").append(module).append(";\n");
    }

    private StringBuilder generateMainBlock(StringBuilder imports) {
        StringBuilder builder = new StringBuilder();
        Set<Type> paramNames = new HashSet<Type>();
        builder.append("Set<Fut<Unit>> ").append(futs).append(" = EmptySet;\n");
        builder.append("Fut<Unit> ").append(fut).append(";\n");
        for (InterfaceDecl key : tests.keySet()) {
            builder.append("//Test cases for ").append(key.getType().getQualifiedName()).append("\n");
            for (ClassDecl clazz : tests.get(key)) {
                builder.append("//Test cases for implementation ").append(clazz.getName()).append("\n");
                paramNames.addAll(generateTestClassImpl(key, clazz, builder));
            }
        }
        generateWaitSync(builder);
        generateImports(imports, paramNames);
        return builder;
    }
    
    private StringBuilder generateWaitSync(StringBuilder builder) {
        return 
          builder
            .append("//waits for methods return...\n")
            .append("while (hasNext(").append(futs).append(")) {\n") // begin while
            .append("Pair<Set<Fut<Unit>>,Fut<Unit>> nt = next(").append(futs).append(");\n")
            .append(fut).append(" = snd(nt);\n")
            .append(futs).append(" = fst(nt);\n")
            .append(fut).append(".get;\n")
            .append("}\n"); // end while
    }

    private Set<Type> generateTestClassImpl(InterfaceDecl inf, ClassDecl clazz, StringBuilder main) {
        Set<Type> paramNames = new HashSet<Type>();
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
                main.append("while (hasNext(").append(dataPointSet).append(")) {\n"); // begin while
                main.append("Pair<Set<").append(dataType).append(">,").append(dataType)
                    .append("> nt = next(").append(dataPointSet).append(");\n");
                main.append(dataType).append(" ").append(dataValue).append(" = snd(nt);\n");
                main.append(dataPointSet).append(" = fst(nt);\n");
            }
            main.append("//Test cases for method ").append(method.getName()).append("\n");
            String objectRef = uncap(namePrefix) + instance;
            main = newCog(main, inf, clazz, objectRef);
            generateAsyncTestCall(main, objectRef, method);
            if (needdata) {
                main.append("}\n"); // end while
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
    private Type generateDataPoints(InterfaceDecl inf, ClassDecl clazz, Set<Type> paramNames, StringBuilder main) {

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
                .append(dataPoint.getName()).append("();\n");

        return data;
    }

    private String dataPointSetName(ClassDecl clazz) {
        return uncap(clazz.getName()) + "dataPointSet";
    }

    /**
     * Find a method defined in {@code inf} that is annotated with [DataPoint],
     * takes no argument and returns a Set of data values.
     * 
     * @param inf
     * @return the method defined in {@code inf} that is annotated with
     *         [DataPoint], or null if such a method does not exist.
     */
    private MethodSig findDataPoints(InterfaceDecl inf) {
        for (MethodSig meth : inf.getAllMethodSigs()) {
            if (hasTestAnnotation(meth.getAnnotations(), dataPointType)) {
                Decl d = ((ParametricDataTypeUse) meth.getReturnType()).getDecl();
                if (d.getType().getQualifiedName().equals(absStdSet)) {
                    return meth;
                }
                return null;
            }
        }
        return null;
    }

    /**
     * Write the line of the form {@code objectRef!method(d);}, where d is
     * {@link #dataValue} to {@code builder}, if {@code method} takes an
     * argument, otherwise it write the line of the form
     * {@code objectRef!method();} to {@code builder}.
     * 
     * @param builder
     * @param objectRef
     * @param method
     */
    private void generateAsyncTestCall(StringBuilder builder, String objectRef, MethodSig method) {
        builder.append(fut).append(" = ").append(objectRef).append("!").append(method.getName());
        Iterator<ParamDecl> paramIts = method.getParamList().iterator();
        if (paramIts.hasNext()) {
            builder.append("(").append(dataValue).append(");\n"); // add
                                                                  // parameter
                                                                  // values
        } else {
            builder.append("();\n"); // no parameter
        }
        builder.append(futs).append("= Insert(").append(fut).append(",").append(futs).append(");\n");
    }

    /**
     * Write the line of the form {@code inf name = new cog clazz();} to
     * {@code builder}
     * 
     * @param builder
     * @param inf
     * @param clazz
     * @param name
     * @return a reference to builder.
     */
    private StringBuilder newCog(StringBuilder builder, InterfaceDecl inf, ClassDecl clazz, String name) {
        return newObj(builder, inf, clazz, name, true);
    }

    /**
     * If {@code cog} is true, this method writes the line of the form
     * {@code inf name = new cog clazz();} to {@code builder}, otherwise it
     * writes the line of the form {@code inf name = new clazz();} to
     * {@code builder}.
     * 
     * @param builder
     * @param inf
     * @param clazz
     * @param name
     * @param cog
     * @return a reference to builder.
     */
    private StringBuilder newObj(StringBuilder builder, InterfaceDecl inf, ClassDecl clazz, String name, boolean cog) {
        return builder.append(inf.getName()).append(" ").append(name).append((cog) ? " = new cog " : " = new ")
                .append(clazz.getName()).append("();\n");
    }

    private Set<MethodSig> getTestMethods(InterfaceDecl inf) {
        Set<MethodSig> testmethods = new HashSet<MethodSig>();
        for (MethodSig meth : inf.getAllMethodSigs()) {
            if (hasTestAnnotation(meth.getAnnotations(), testType)) {
                testmethods.add(meth);
            }
        }
        return testmethods;
    }

}
