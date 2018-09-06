/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.tests;

import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
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
import abs.frontend.ast.MethodImpl;
import abs.frontend.ast.MethodSig;
import abs.frontend.ast.Model;
import abs.frontend.ast.ParametricDataTypeDecl;
import abs.frontend.ast.ParametricDataTypeUse;
import abs.frontend.ast.PureExp;

/**
 *
 * @author pwong
 *
 */
abstract class AbstractABSTestRunnerGenerator implements ABSTestRunnerGenerator {

    protected static final String ignore = "AbsUnit.Ignored";
    protected static final String test = "AbsUnit.Test";
    protected static final String dataPoint = "AbsUnit.DataPoint";

    protected static final String suite = "AbsUnit.Suite";
    protected static final String fixture = "AbsUnit.Fixture";
    protected static final String absStdSet = "ABS.StdLib.Set";
    protected static final String absStdLib = "ABS.StdLib";

    protected static final String dataValue = "d";
    protected static final String futs = "futs";
    protected static final String fut = "fut";

    protected DataConstructor ignoreType;
    protected DataConstructor testType;
    protected DataConstructor dataPointType;

    protected DataConstructor suiteType;
    protected DataConstructor fixtureType;

    protected Map<InterfaceDecl, Set<ClassDecl>> tests = new HashMap<>();

    protected final Model model;

    protected boolean isEmpty = true;


    /**
     * The constructor takes a type checked {@link Model} of the ABS model
     *
     * @param model
     * @throws IllegalArgumentException if model is null
     */
    protected AbstractABSTestRunnerGenerator(Model model) {
        if (model == null)
            throw new IllegalArgumentException("Model cannot be null!");

        this.model = model;

        gatherABSUnitAnnotations();

        /*
         * Do not search for test class definitions if this model does not
         * contain the necessary ABSUnit annotations
         */
        if (ignoreType == null || testType == null || dataPointType == null ||
            suiteType == null || fixtureType == null) {
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

    private InterfaceDecl getTestClass(ClassDecl clazz) {
        for (InterfaceTypeUse inf : clazz.getImplementedInterfaceUseList()) {
            if (inf.getDecl() instanceof InterfaceDecl) {
                InterfaceDecl decl = (InterfaceDecl) inf.getDecl();
                if (hasTestAnnotation(decl.getAnnotations(), fixtureType) &&
                    ! hasTestAnnotation(decl.getAnnotations(), ignoreType)) {
                    return decl;
                }
            }
        }
        return null;
    }

    private boolean isTestClassImpl(ClassDecl clazz) {
        return hasTestAnnotation(clazz.getAnnotations(), suiteType);
    }

    private void addTest(InterfaceDecl inf, ClassDecl clazz) {
        if (!tests.containsKey(inf)) {
            tests.put(inf, new HashSet<>());
        }
        tests.get(inf).add(clazz);
    }

    /**
     * Checks if this generator contains a {@link Model} that defines ABSUnit
     * tests.
     *
     * @return
     */
    @Override
    public boolean hasUnitTest() {
        return !isEmpty;
    }

    protected void gatherABSUnitAnnotations() {
        for (Decl decl : this.model.getDecls()) {
            if (decl instanceof ParametricDataTypeDecl) {
                String name = decl.getType().getQualifiedName();
                if (test.equals(name)) {
                    testType = ((ParametricDataTypeDecl) decl).getDataConstructor(0);
                } else if (fixture.equals(name)) {
                    fixtureType = ((ParametricDataTypeDecl) decl).getDataConstructor(0);
                } else if (suite.equals(name)) {
                    suiteType = ((ParametricDataTypeDecl) decl).getDataConstructor(0);
                } else if (dataPoint.equals(name)) {
                    dataPointType = ((ParametricDataTypeDecl) decl).getDataConstructor(0);
                } else if (ignore.equals(name)) {
                    ignoreType = ((ParametricDataTypeDecl) decl).getDataConstructor(0);
                }
            }
        }
    }

    protected void gatherTestClasses() {
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

    protected boolean hasTestAnnotation(List<Annotation> annotations, DataConstructor... constructors) {
        java.util.List<DataConstructor> cs = Arrays.asList(constructors);
        for (Annotation ta : annotations) {
            PureExp exp = ta.getValue();
            if (exp instanceof DataConstructorExp
                    && cs.contains(((DataConstructorExp) exp).getDataConstructor())) {
                return true;
            }
        }
        return false;
    }


    protected String uncap(String word) {
        return new StringBuilder().append(Character.toLowerCase(word.charAt(0))).append(word.substring(1)).toString();
    }

    protected String dataPointSetName(ClassDecl clazz) {
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
    protected MethodSig findDataPoints(InterfaceDecl inf) {
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

    protected Set<MethodSig> getTestMethods(InterfaceDecl inf) {
        Set<MethodSig> testmethods = new HashSet<>();
        for (MethodSig meth : inf.getAllMethodSigs()) {
            /*
             * Add those methods that are tests but are not ignored
             */
            if (hasTestAnnotation(meth.getAnnotations(), testType) &&
                ! hasTestAnnotation(meth.getAnnotations(), ignoreType)) {
                testmethods.add(meth);
            }
        }
        return testmethods;
    }

    protected boolean isIgnored(ClassDecl clazz, MethodSig method) {
        for (MethodImpl m : clazz.getMethodList()) {
            if (m.getMethodSig().getName().equals(method.getName())) {
                return hasTestAnnotation(m.getMethodSig().getAnnotationList(), ignoreType);
            }
        }
        return false;
    }
}
